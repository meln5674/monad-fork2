{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Sums where

import Control.Exception
import Control.Concurrent

import Control.Monad.Trans
import Control.Monad.Writer.Strict
import Control.Monad.Except
import Control.Monad.Reader

import Control.Monad.Fork

newtype ExampleT m a = ExampleT { runExampleT :: WriterT (Sum Int) (ExceptT String (ReaderT ([Int], Int) m)) a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadError String
           , MonadWriter (Sum Int)
           , MonadReader ([Int], Int)
           )

instance MonadFork h m => MonadFork ((Sum Int -> m ()) :<: (String -> m ()) :<: h) (ExampleT m) where
    forkT (writerHandler :<: exceptHandler :<: subHandler) (ExampleT action) 
        = ExampleT $ forkT (writerHandler' :<: exceptHandler' :<: subHandler) action
      where
        writerHandler' :: Sum Int -> ExceptT String (ReaderT ([Int], Int) _) ()
        writerHandler' x = lift $ lift $ writerHandler x
        exceptHandler' :: String -> ReaderT ([Int], Int) _ ()
        exceptHandler' x = lift $ exceptHandler x

data WorkerResult a
    = WorkerFailed String
    | WorkerDone a

runWorker :: Int -> Int -> ExampleT IO ()
runWorker start count = do 
    (input, _) <- ask
    let workerShare = map Sum $ take count (drop start input)
    when (null workerShare) $ throwError "Empty worker"
    mapM tell workerShare
    return ()

makeWorker :: Int -> Int -> ExampleT IO (ThreadId, MVar (WorkerResult (Sum Int)))
makeWorker start count = do
    workerVar <- liftIO $ newEmptyMVar
    let handleExcept msg = putMVar workerVar $ WorkerFailed msg
        handleWriter result = putMVar workerVar $ WorkerDone result
    threadId <- forkT (handleWriter :<: handleExcept :<: ()) $ runWorker start count
    return (threadId, workerVar)

makeWorkers :: Int -> ExampleT IO [(ThreadId, MVar (WorkerResult (Sum Int)))]
makeWorkers numWorkers = do
    (_, inputLength) <- ask
    let perWorker = inputLength `div` numWorkers
    forM [0, perWorker .. inputLength] $ \start -> makeWorker start perWorker

combineWorkerResults :: [MVar (WorkerResult (Sum Int))] -> ExampleT IO ()
combineWorkerResults vars = forM_ vars $ \var -> do
    workerResult <- liftIO $ readMVar var
    case workerResult of
        WorkerFailed msg -> throwError msg
        WorkerDone result -> tell result

main :: IO ()
main = do
    let input = [0..5000]
        inputLength = 5001
        numWorkers = 10
    result <- flip runReaderT (input, inputLength) 
                 $ runExceptT 
                 $ runWriterT 
                 $ runExampleT
                 $ do
                    workers <- makeWorkers numWorkers
                    let workerVars = map snd workers
                    combineWorkerResults workerVars
    case result of
        Right ((), Sum x) -> putStrLn $ "Sum is " ++ show x
        Left msg -> putStrLn $ "Failed with error: " ++ show msg
