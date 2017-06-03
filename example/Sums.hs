--------------------------------------------------------------------------------
-- |
-- 
-- An example use of `Control.Monad.Fork`
--
-- This program performs a simple task, summing up integers, within a relatively
-- complex monad transformer stack, using except for handling errors, writer for
-- maintaining the total, and reader to access the integers to sum, along with
-- the number that exist
-- 
-- This stack is then wrapped in a newtype, `ExampleT`, which is given instances
-- for the standard mtl classes, as well as `MonadFork`.
--
-- Each worker is provided with a offset and count in the list, and the forking
-- handlers report back to the main thread using an MVar.
--
-- After the worker threads are created, the main thread waits on each MVar,
-- then either throws or tells based on the result
--------------------------------------------------------------------------------

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Sums where

import Control.Exception
import Control.Concurrent

import Control.Monad.Trans
import Control.Monad.Writer.Strict
import Control.Monad.Except
import Control.Monad.Reader

import Control.Monad.Fork

newtype ExampleT m a = ExampleT { runExampleInnerT :: WriterT (Sum Int) (ExceptT String (ReaderT ([Int], Int) m)) a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadError String
           , MonadWriter (Sum Int)
           , MonadReader ([Int], Int)
           )
type ExampleTWriterHandler m = Sum Int -> ExceptT String (ReaderT ([Int], Int) m) ()
type ExampleTExceptHandler m = String -> ReaderT ([Int], Int) m ()

runExampleT :: ExampleT m a -> [Int] -> Int -> m (Either String (a, Sum Int))
runExampleT f input inputLength = runReaderT (runExceptT (runWriterT (runExampleInnerT f))) (input, inputLength)

deriving instance MonadFork h m => MonadFork 
    (ExampleTWriterHandler m :<: ExampleTExceptHandler m :<: h)
    (ExampleT m)

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
    let handleExcept :: ExampleTExceptHandler IO
        handleExcept msg = liftIO $ putMVar workerVar $ WorkerFailed msg
        handleWriter :: ExampleTWriterHandler IO
        handleWriter result = liftIO $ putMVar workerVar $ WorkerDone result
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

doMain :: Int -> ExampleT IO ()
doMain numWorkers = do
    workers <- makeWorkers numWorkers
    let workerVars = map snd workers
    combineWorkerResults workerVars

main :: IO ()
main = do
    let input = [0..5000]
        inputLength = 5001
        numWorkers = 10
    result <- runExampleT (doMain numWorkers) input inputLength
    case result of
        Right ((), Sum x) -> putStrLn $ "Sum is " ++ show x
        Left msg -> putStrLn $ "Failed with error: " ++ show msg
