-- | Functions to use the Job daemon and its queue.
module JobsDaemon.Util (
    -- * Daemon processing queue management
      newQueue, registerJob, enqueueJob
    -- * Others utilities
    , runDBIO
    ) where

import Prelude
import Yesod

import Control.Applicative ((<$>))
import Control.Concurrent (
      newChan, writeChan, newMVar, modifyMVar_
    )
import qualified Control.Exception as E
import Control.Monad
import Control.Monad.Logger (NoLoggingT, runNoLoggingT)
import qualified Data.Map as M
import Data.Time.Clock (getCurrentTime)

import Control.Monad.Trans.Resource (ResourceT, runResourceT)

import Foundation (App (..))
import JobsDaemon.Type
import Model

-- | Initialises a new empty job queue to be inserted in the foundation type.
newQueue :: IO JobsQueue
newQueue = do
    chan <- JobsChan <$> newChan
    mvar <- newMVar (JobsDepends M.empty)
    return $ JobsQueue chan mvar

-- | Registers a job to the queue and to the database. Returns the id of the
-- database inserted object.
-- The action will be executed after each of its dependencies has been
-- completed.
registerJob :: App -> FileId -> JobType
            -> [JobId] -- ^ Job\'s dependencies.
            -> IO ()   -- ^ The job to be executed in background.
            -> IO JobId
registerJob app fileId typ deps action = do
    -- Saves the job and its dependencies graph to the database.
    time <- getCurrentTime
    let job = Job {
              jobFile = fileId, jobType = typ, jobCreated = time
            , jobCompleted = False, jobCpuTime = Nothing, jobException = Nothing
            }
    jobId <- runDBIO app $ do
        jobId <- insert job
        forM_ deps $
            insert . JobDependency jobId
        return jobId

    -- Enqueues the job.
    enqueueJob app jobId deps action

    return jobId

-- | Adds a job to the queue. Supposes the job is already registered in the
-- database (useful to restore the processing queue after a shutdown).
-- The action will be executed after each of its dependencies has been
-- completed.
-- A job must always depend on previously created jobs (ie must always depend
-- on lower JobId than its own JobId).
enqueueJob :: App -> JobId -> [JobId] -> IO () -> IO ()
enqueueJob app jobId deps action = assertOrdered $ do
    let JobsQueue (JobsChan chan) mvar = jobsQueue app

    modifyMVar_ mvar $ \(JobsDepends graph) ->
        let deps'  = filter (`M.member` graph) deps -- Uncompleted parents.
            graph' = M.insert jobId [] graph
        in if null deps'
            then do -- Starts the job as soon as a thread is available.
                writeChan chan (jobId, action)
                return $! JobsDepends graph'
            else do -- Push the job in the waiting list of its first dependency.
                let (d:ds) = deps'
                    entry = (jobId, action, ds)
                return $! JobsDepends $! M.adjust (entry :) d graph'
  where
    -- Asserts that every dependencies has been created before the job being
    -- inserted.
    assertOrdered = E.assert (all (< jobId) deps)

-- | Runs a transaction inside the IO monad.
runDBIO :: App -> YesodPersistBackend App (NoLoggingT (ResourceT IO)) a -> IO a
runDBIO app f =
    runResourceT $ runNoLoggingT $ runPool (persistConfig app) f (connPool app)
