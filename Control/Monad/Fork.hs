{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Control.Monad.Fork where

import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import qualified Control.Monad.State.Lazy as Lazy
import qualified Control.Monad.State.Strict as Strict
import Control.Monad.Reader
import Control.Monad.Except

import Control.Concurrent

infixl 1 :>:
data a :>: b = a :>: b

class Monad m => MonadFork m where
    type ForkHandler m
    forkT :: ForkHandler m -> m () -> m ThreadId

instance MonadFork IO where
    type ForkHandler IO = ()
    forkT _ = forkIO

instance MonadFork m => MonadFork (ReaderT r m) where
    type ForkHandler (ReaderT r m) = ForkHandler m
    forkT subHandler action = ReaderT $ \env -> 
        forkT subHandler (runReaderT action env)

instance MonadFork m => MonadFork (MaybeT m) where
    type ForkHandler (MaybeT m) = m () :>: ForkHandler m
    forkT (onNothing :>: subHandler) action = MaybeT $ fmap Just $ forkT subHandler $
        runMaybeT action >>= \result -> case result of
            Just () -> return ()
            Nothing -> onNothing

instance MonadFork m => MonadFork (ExceptT e m) where
    type ForkHandler (ExceptT e m) = (e -> m ()) :>: ForkHandler m
    forkT (onException :>: subHandler) action = ExceptT $ fmap Right $ forkT subHandler $
        runExceptT action >>= \result -> case result of
            Right () -> return ()
            Left ex -> onException ex

instance MonadFork m => MonadFork (Lazy.StateT s m) where
    type ForkHandler (Lazy.StateT s m) = (s -> s -> m ()) :>: ForkHandler m
    forkT (reportState :>: subHandler) action = Lazy.StateT $ \s -> fmap (flip (,) s) $ forkT subHandler $
        Lazy.runStateT action s >>= \((), s') -> reportState s s'

instance MonadFork m => MonadFork (Strict.StateT s m) where
    type ForkHandler (Strict.StateT s m) = (s -> s -> m ()) :>: ForkHandler m
    forkT (reportState :>: subHandler) action = Strict.StateT $ \s -> fmap (flip (,) s) $ forkT subHandler $
        Strict.runStateT action s >>= \((), s') -> reportState s s'
