{-|
Module      : Control.Monad.Fork
Description : Multithreading within monad transformer stacks
Copyright   : (c) Andrew Melnick 2016
License     : BSD3
Maintainer  : Andrew Melnick
Stability   : experimental
Portability : portable

Lifting the concept of forking a new thread into common monad transformers

Monads in the MonadFork class are able to run actions in a new thread, but
require some sort of "handler" data in order to do so properly, for example,
the `ExceptT` Transformer requires some action to be run in the event that
the new thread has an exception

The `:<:` operator joins handlers into a chain, so the monad
@(AT (BT (CT IO)))@ will have a handler that looks like
@(A :<: B :<: C :<: ())@, (where `()` is the handler for `IO`), indicating that
the handlers will be applied in the same order as the effects if one were
unwrapping the stack using @runCT (runBT (runAT x)))@
-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Control.Monad.Fork where

import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import qualified Control.Monad.State.Lazy as Lazy
import qualified Control.Monad.State.Strict as Strict
import qualified Control.Monad.Writer.Lazy as Lazy
import qualified Control.Monad.Writer.Strict as Strict
import qualified Control.Monad.RWS.Lazy as Lazy
import qualified Control.Monad.RWS.Strict as Strict
import Control.Monad.Reader
import Control.Monad.Except

import Control.Monad.Catch


import Control.Concurrent

infixr 1 :<:

-- | Monads whose actions can be run in a new thread, provided a handler of
-- type `h` is provided
class Monad m => MonadFork h m where
    -- | Run an action in a new thread, and return the id of the new thread
    forkT :: h -> m () -> m ThreadId

-- | Monads whose actions can not only be run in a new thread, but are able to
-- react to asynchronous exceptions being thrown in the thread
class (MonadMask m, MonadFork h m) => MonadForkFinally h m where
    -- | Like `forkT`, but also takes an action to execute on thread completion
    --
    -- A default implementation is available based on the relationship between
    -- `forkIO` and `forkFinally`, however, that implementation will 
    -- A) Only execute the handler if no exception is thrown, and B)
    -- Execute the handler before the thread completion handler, so any effects
    -- caused by it will be lost
    forkFinallyT :: h -> m a -> (Either SomeException a -> m ()) -> m ThreadId
    forkFinallyT handler action and_then = mask $ \restore ->
        forkT handler $ try (restore action) >>= and_then

-- | Base of the instance stack, handler does nothing
instance MonadFork () IO where
    forkT _ = forkIO

instance MonadForkFinally () IO where
    forkFinallyT _ = forkFinally
    

-- | Environment is provided to the new thread, handler does nothing
instance MonadFork h m => MonadFork h (ReaderT r m) where
    forkT subHandler action = ReaderT $ \env -> 
        forkT subHandler (runReaderT action env)

instance (MonadMask m, MonadForkFinally h m) => MonadForkFinally h (ReaderT r m)

-- | Handler should report a `Nothing` result
instance MonadFork h m => MonadFork (m () :<: h) (MaybeT m) where
    forkT (onNothing :<: subHandler) action = MaybeT $ fmap Just $ forkT subHandler $
        runMaybeT action >>= \result -> case result of
            Just () -> return ()
            Nothing -> onNothing

-- | Handler should report a `Left` result
instance MonadFork h m => MonadFork ((e -> m ()) :<: h) (ExceptT e m) where
    forkT (onException :<: subHandler) action = ExceptT $ fmap Right $ forkT subHandler $
        runExceptT action >>= \result -> case result of
            Right () -> return ()
            Left ex -> onException ex

-- | Handler should report original and final state
instance MonadFork h m => MonadFork ((s -> s -> m ()) :<: h) (Lazy.StateT s m) where
    forkT (reportState :<: subHandler) action = Lazy.StateT $ \s -> fmap (flip (,) s) $ forkT subHandler $
        Lazy.runStateT action s >>= \((), s') -> reportState s s'

instance (MonadMask m, MonadForkFinally h m) => MonadForkFinally ((s -> s -> m ()) :<: h) (Lazy.StateT s m)

-- | Handler should report original and final state
instance MonadFork h m => MonadFork ((s -> s -> m ()) :<: h) (Strict.StateT s m) where
    forkT (reportState :<: subHandler) action = Strict.StateT $ \s -> fmap (flip (,) s) $ forkT subHandler $
        Strict.runStateT action s >>= \((), s') -> reportState s s'

instance (MonadMask m, MonadForkFinally h m) => MonadForkFinally ((s -> s -> m ()) :<: h) (Strict.StateT s m)

-- | Handler should report final monoidal value
instance (Monoid w, MonadFork h m) => MonadFork ((w -> m ()) :<: h) (Lazy.WriterT w m) where
    forkT (reportLog :<: subHandler) action = Lazy.WriterT $ fmap (flip (,) mempty) $ forkT subHandler $
        Lazy.runWriterT action >>= \((), w) -> reportLog w

instance (Monoid w, MonadMask m, MonadForkFinally h m) => MonadForkFinally ((w -> m ()) :<: h) (Lazy.WriterT w m)

-- | Handler should report final monoidal value
instance (Monoid w, MonadFork h m) => MonadFork ((w -> m ()) :<: h) (Strict.WriterT w m) where
    forkT (reportLog :<: subHandler) action = Strict.WriterT $ fmap (flip (,) mempty) $ forkT subHandler $
        Strict.runWriterT action >>= \((), w) -> reportLog w

instance (Monoid w, MonadMask m, MonadForkFinally h m) => MonadForkFinally ((w -> m ()) :<: h) (Strict.WriterT w m)

-- | Environment is provided to the new thread, handler should report final monoid value, initial state, and final state
instance (Monoid w, MonadFork h m) => MonadFork ((w -> s -> s -> m ()) :<: h) (Lazy.RWST r w s m) where
    forkT (reportLogAndState :<: subHandler) action = Lazy.RWST $ \r s -> fmap (\x -> (x, s, mempty)) $ forkT subHandler $
        Lazy.runRWST action r s >>= \((), s', w) -> reportLogAndState w s s'

instance (Monoid w, MonadMask m, MonadForkFinally h m) => MonadForkFinally ((w -> s -> s -> m ()) :<: h) (Lazy.RWST r w s m)

-- | Environment is provided to the new thread, handler should report final monoid value, initial state, and final state
instance (Monoid w, MonadFork h m) => MonadFork ((w -> s -> s -> m ()) :<: h) (Strict.RWST r w s m) where
    forkT (reportLogAndState :<: subHandler) action = Strict.RWST $ \r s -> fmap (\x -> (x, s, mempty)) $ forkT subHandler $
        Strict.runRWST action r s >>= \((), s', w) -> reportLogAndState w s s'

instance (Monoid w, MonadMask m, MonadForkFinally h m) => MonadForkFinally ((w -> s -> s -> m ()) :<: h) (Strict.RWST r w s m)

-- | A handler chain, a is used to handle the top of the stack, b is then used 
-- to handle the rest of the stack
data a :<: b = a :<: b

-- | Convenience type family for referring to handlers
type family ForkHandler (m :: * -> *)
type instance ForkHandler IO = ()
type instance ForkHandler (ReaderT r m) = ForkHandler m
type instance ForkHandler (MaybeT m) = m () :<: ForkHandler m
type instance ForkHandler (ExceptT e m) = (e -> m ()) :<: ForkHandler m
type instance ForkHandler (Lazy.StateT s m) = (s -> s -> m ()) :<: ForkHandler m
type instance ForkHandler (Strict.StateT s m) = (s -> s -> m ()) :<: ForkHandler m
type instance ForkHandler (Lazy.WriterT w m) = (w -> m ()) :<: ForkHandler m
type instance ForkHandler (Strict.WriterT w m) = (w -> m ()) :<: ForkHandler m
type instance ForkHandler (Lazy.RWST r w s m) = (w -> s -> s -> m ()) :<: ForkHandler m
type instance ForkHandler (Strict.RWST r w s m) = (w -> s -> s -> m ()) :<: ForkHandler m
