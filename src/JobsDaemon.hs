-- | Defines a daemon which listens and executes the jobs added to a queue.
module JobsDaemon (
    -- * Daemon processing queue management
      JobsQueue, newQueue, putJob
    -- * Starting the daemon
    , jobsDaemon, forkJobsDaemon
    -- * Utilities
    , runDBIO
    ) where

import Import

import Control.Concurrent (ThreadId, Chan, forkIO, newChan, writeChan, readChan)
import qualified Control.Exception as E
import Control.Monad

import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Database.Persist.Store (runPool)

type Job = IO ()
type JobsQueue = Chan Job

-- | Initialises a new empty job queue to be inserted in the foundation type.
newQueue :: IO JobsQueue
newQueue = newChan

-- | Adds a job to the queue.
putJob :: App -> IO () -> IO ()
putJob = writeChan . jobsQueue

-- | Waits for a job from the queue and executes it. Nevers returns.
jobsDaemon :: App -> IO ()
jobsDaemon app = do
    let queue = jobsQueue app
    forever $ do
        action <- readChan queue
        E.try action :: IO (Either E.SomeException ())

-- | Forks the jobs daemon on a new thread and returns its 'ThreadId'.
forkJobsDaemon :: App -> IO ThreadId
forkJobsDaemon = forkIO . jobsDaemon

-- | Runs a transaction inside the IO monad.
runDBIO :: App -> YesodPersistBackend App (ResourceT IO) a -> IO a
runDBIO app f = runResourceT $ runPool (persistConfig app) f (connPool app)
