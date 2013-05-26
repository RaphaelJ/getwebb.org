{-# LANGUAGE ScopedTypeVariables #-}
-- | Defines a daemon which listens and executes the jobs added to a queue.
-- The queue supports multiple dependencies between jobs and provides features
-- which enable it to be restored from an application shutdown using the
-- database.
module JobsDaemon (
    -- * Daemon processing queue management
      JobsQueue, JobsChan, JobsDepends, newQueue, registerJob, enqueueJob
    -- * Starting the daemon
    , jobsDaemon, forkJobsDaemon
    -- * Utilities
    , runDBIO
    ) where

import Prelude
import Yesod

import Control.Concurrent (
      ThreadId, forkIO
    , newChan, writeChan, readChan, newMVar, modifyMVar, modifyMVar_
    )
import qualified Control.Exception as E
import Control.Monad
import Control.Monad.Logger (NoLoggingT, runNoLoggingT)
import Data.List (foldl')
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)

import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Database.Persist.Store (runPool)
import System.TimeIt (timeItT)

import Foundation (
      App, JobsQueue, JobsChan, JobsDepends
    , persistConfig, jobsQueue, connPool
    )
import Model

-- | Initialises a new empty job queue to be inserted in the foundation type.
newQueue :: IO JobsQueue
newQueue = do
    chan <- newChan
    mvar <- newMVar (M.empty)
    return (chan, mvar)

-- | Registers a job to the queue and to the database. Returns the id of the
-- database inserted object.
-- The action will be executed after each of its dependencies has been
-- completed.
registerJob :: App -> FileId -> JobType
            -> [JobId] -- ^ Job\'s dependencies.
            -> IO () -- ^ The job to be executed in background.
            -> IO JobId
registerJob app fileId typ deps action = do
    -- Commits the job and its dependencies graph to the database
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
    let (chan, mvar) = jobsQueue app

    modifyMVar_ mvar $ \graph ->
        let deps' = filter (`M.member` graph) deps -- Uncompleted parents.
            graph' = M.insert jobId [] graph
        in if null deps'
            then do -- Starts the job as soon as a thread slot is available.
                writeChan chan (jobId, action)
                return graph'
            else do -- Push the job in the waiting list of the first dependency.
                let (d:ds) = deps'
                    entry = (jobId, action, ds)
                return $! M.adjust (entry :) d graph'
  where
    -- Asserts that every dependencies has been created before the job being
    -- inserted.
    assertOrdered = E.assert (all (< jobId) deps)

-- | Waits for a job from the queue and executes it. Never returns.
jobsDaemon :: App -> IO ()
jobsDaemon app = do
    let (chan, mvar) = jobsQueue app

    forever $ do
        (jobId, action) <- readChan chan

        (time, eExcept) <- timeItT $ E.try action

        let except = case eExcept of
                Left (e :: E.SomeException) -> Just $ T.pack $ show e
                Right _                     -> Nothing

        runDBIO app $ do
            update jobId [ JobCompleted =. True, JobCpuTime =. Just time
                         , JobException =. except ]

        -- Unlocks dependent jobs.
        ready <- modifyMVar mvar $ \graph ->
            let Just deps = jobId `M.lookup` graph
            in return $! foldl' step (graph, []) deps

        forM_ ready (writeChan chan)
  where
    -- Adds a job to the dependency graph of its first not finished parent or
    -- to the ready list if there is no more unexecuted dependency.
    step (graph, ready) (jobId, action, deps) =
        let deps' = filter (`M.member` graph) deps
        in if null deps'
            then (graph, (jobId, action) : ready)
            else let (d:ds) = deps'
                     entry = (jobId, action, ds)
                 in (M.adjust (entry :) d graph, ready)

-- | Forks the jobs daemon on a new thread and returns its 'ThreadId'.
forkJobsDaemon :: App -> IO ThreadId
forkJobsDaemon = forkIO . jobsDaemon

-- | Runs a transaction inside the IO monad.
runDBIO :: App -> YesodPersistBackend App (NoLoggingT (ResourceT IO)) a -> IO a
runDBIO app f =
    runResourceT $ runNoLoggingT $ runPool (persistConfig app) f (connPool app)
