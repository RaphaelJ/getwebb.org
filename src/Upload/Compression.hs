-- | This module declares a daemon which will receive file to compress on a
-- concurrent queue. Try to compress the file using the GZip algorithm and
-- update the database entry if the compressed file is smaller than the original
-- file.
module Upload.Compression (
    -- * Processing queue management
      CompressionQueue, newQueue, putFile
    -- * Starting the daemon
    , compressionDaemon, forkCompressionDaemon
    ) where

import Import

import Data.Maybe
import Control.Concurrent
import Control.Concurrent.Chan
import System.IO

import Database.Persist.Store (runPool)

import Upload.Processing (hashPath)

type CompressionQueue = Chan FileId

-- | Waits files to compress on the concurrent queue and process them. Never
-- returns.
compressionDaemon :: App -> IO ()
compressionDaemon app =
    forever $ do
        -- Waits until an FileId has been insered in the queue.
        fileId <- readChan $ compressionQueue app

        mFile <- runTransact $ get fileId
        when (isJust mFile) $ do
            -- Process the file if it still exists.
            let hash = fileSha1 $ fromJust mFile
            let path = hashPath uploadDir hash </> "original"

  where
    runTransact f = runPool (persistConfig app) f (connPool app)
    uploadDir = extraUploadDir $ appExtra $ settings app

-- | Forks the compression daemon on a new thread and returns its 'ThreadId'.
forkCompressionDaemon :: App -> IO ThreadId
forkCompressionDaemon = forkIO . compressionDaemon

-- | Initialises a new compression queue to be insered in the foundation type.
newQueue :: IO CompressionQueue
newQueue = newChan

-- | Adds a file to the compression queue.
putFile :: App -> FileId -> IO ()
putFile = writeChan . compressionQueue