--------------------------------------------------------------------------------
-- |
-- Module : Control.Monad.Fork
-- Copyright : (c) Andrew Melnick 2016
-- License : BSD3
-- 
-- Maintaner : Andrew Melnick
-- Stability : experimental
-- Portability : portable
--
-- Lifting the concept of forking a new thread into common monad transformers
--
-- Monads in the MonadFork class are able to run actions in a new thread, but
-- require some sort of "handler" data in order to do so properly, for example,
-- the `ExceptT` Transformer requires some action to be run in the event that
-- the new thread has an exception
--
-- The `:<:` operator joins handlers into a chain, so the monad
-- @(AT (BT (CT IO)))@ will have a handler that looks like
-- @(A :<: B :<: C :<: ())@, (where `()` is the handler for `IO`), indicating that
-- the handlers will be applied in the same order as the effects if one were
-- unwrapping the stack using @runCT (runBT (runAT x)))@
--------------------------------------------------------------------------------
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

import Control.Concurrent

infixr 1 :<:

-- | Monads whose actions can be run in a new thread, provided a handler of
-- type `h` is provided
class Monad m => MonadFork h m where
    -- | Run an action in a new thread, and return the id of the new thread
    forkT :: h -> m () -> m ThreadId

-- | Base of the instance stack, handler does nothing
instance MonadFork () IO where
    forkT _ = forkIO

-- | Environment is provided to the new thread, handler does nothing
instance MonadFork h m => MonadFork h (ReaderT r m) where
    forkT subHandler action = ReaderT $ \env -> 
        forkT subHandler (runReaderT action env)

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

-- | Handler should report original and final state
instance MonadFork h m => MonadFork ((s -> s -> m ()) :<: h) (Strict.StateT s m) where
    forkT (reportState :<: subHandler) action = Strict.StateT $ \s -> fmap (flip (,) s) $ forkT subHandler $
        Strict.runStateT action s >>= \((), s') -> reportState s s'

-- | Handler should report final monoidal value
instance (Monoid w, MonadFork h m) => MonadFork ((w -> m ()) :<: h) (Lazy.WriterT w m) where
    forkT (reportLog :<: subHandler) action = Lazy.WriterT $ fmap (flip (,) mempty) $ forkT subHandler $
        Lazy.runWriterT action >>= \((), w) -> reportLog w

-- | Handler should report final monoidal value
instance (Monoid w, MonadFork h m) => MonadFork ((w -> m ()) :<: h) (Strict.WriterT w m) where
    forkT (reportLog :<: subHandler) action = Strict.WriterT $ fmap (flip (,) mempty) $ forkT subHandler $
        Strict.runWriterT action >>= \((), w) -> reportLog w

-- | Environment is provided to the new thread, handler should report final monoid value, initial state, and final state
instance (Monoid w, MonadFork h m) => MonadFork ((w -> s -> s -> m ()) :<: h) (Lazy.RWST r w s m) where
    forkT (reportLogAndState :<: subHandler) action = Lazy.RWST $ \r s -> fmap (\x -> (x, s, mempty)) $ forkT subHandler $
        Lazy.runRWST action r s >>= \((), s', w) -> reportLogAndState w s s'

-- | Environment is provided to the new thread, handler should report final monoid value, initial state, and final state
instance (Monoid w, MonadFork h m) => MonadFork ((w -> s -> s -> m ()) :<: h) (Strict.RWST r w s m) where
    forkT (reportLogAndState :<: subHandler) action = Strict.RWST $ \r s -> fmap (\x -> (x, s, mempty)) $ forkT subHandler $
        Strict.runRWST action r s >>= \((), s', w) -> reportLogAndState w s s'

-- | A handler chain, a is used to handle the top of the stack, b is then used 
-- to handle the rest of the stack
data a :<: b = a :<: b

