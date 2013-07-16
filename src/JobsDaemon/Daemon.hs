{-# LANGUAGE ScopedTypeVariables #-}
-- | Daemon which listens and executes the jobs added to its queue.
-- The queue supports multiple dependencies between jobs and can be restored
-- from an application shutdown using the database.
-- One queue can be shared by multiple daemons to accelerate the processing.
module JobsDaemon.Daemon (
    -- * Starting the daemon
      jobsDaemon, forkJobsDaemon
    ) where

import Prelude
import Yesod

import Control.Concurrent (
      ThreadId, forkIO, writeChan, readChan, modifyMVar_
    )
import qualified Control.Exception as E
import Control.Monad
import Data.List (foldl')
import qualified Data.Map as M
import qualified Data.Text as T

import System.TimeIt (timeItT)

import Foundation (App (..))
import JobsDaemon.Util (runDBIO)
import JobsDaemon.Type
import Model

-- | Waits for a job from the queue and executes it. Never returns.
jobsDaemon :: App -> IO ()
jobsDaemon app = do
    let JobsQueue (JobsChan chan) mvar = jobsQueue app

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
        modifyMVar_ mvar $ \(JobsDepends graph) -> do
            let Just deps        = jobId `M.lookup` graph
                graph'           = jobId `M.delete` graph
                (graph'', ready) = foldl' step (graph', []) deps
            forM_ ready (writeChan chan)
            return $ JobsDepends graph''
  where
    -- Adds a job to the dependency graph of its first not finished parent or
    -- to the ready list if there is no more unexecuted dependency.
    step (graph, ready) (jobId, action, deps) =
        let deps' = filter (`M.member` graph) deps -- Uncompleted dependencies.
        in if null deps'
            then (graph, (jobId, action) : ready)
            else let (d:ds) = deps'
                     entry  = (jobId, action, ds)
                 in (M.adjust (entry :) d graph, ready)

-- | Forks the jobs daemon @n@ times. Returns @n@ 'ThreadId's.
forkJobsDaemon :: Int -> App -> IO [ThreadId]
forkJobsDaemon n = replicateM n . forkIO . jobsDaemon
