-- | This module declares a daemon which will receive file to compress on a
-- concurrent queue. Try to compress the file using the GZip algorithm and
-- update the database entry if the compressed file is smaller than the original
-- file.
module Upload.Compression (
    -- * Daemon processing queue management
      CompressionQueue, newQueue, putFile
    -- * Starting the daemon
    , compressionDaemon, forkCompressionDaemon
    ) where

import Import

import Control.Concurrent (ThreadId, Chan, forkIO, newChan, writeChan, readChan)
import Control.Monad
import Data.Maybe
import qualified Data.Text as T
import System.Directory
import System.IO

import Control.Monad.Trans.Resource (ResourceT, runResourceT, register)
import Database.Persist.Query.Internal (selectKeysList)
import Database.Persist.Store (runPool)
import Data.Conduit (($$), ($=))
import Data.Conduit.Binary (sourceFile, sinkHandle)
import Data.Conduit.Zlib (gzip)

import Upload.Path (ObjectType (..), hashDir, uploadDir, newTmpFile, getPath)

import System.TimeIt (timeIt)
import Text.Printf

type CompressionQueue = Chan FileId

-- | Initialises a new compression queue to be inserted in the foundation type.
newQueue :: IO CompressionQueue
newQueue = newChan

-- | Adds a file to the compression queue.
putFile :: App -> FileId -> IO ()
putFile = writeChan . compressionQueue

-- | Waits files to compress on the concurrent queue and process them. Never
-- returns.
compressionDaemon :: App -> IO ()
compressionDaemon app = do
    let queue = compressionQueue app

    -- Reload the previous state of the queue
    fs <- runDBIO $ selectKeysList [FileCompressionQueue ==. True] [Asc FileId]
    forM_ fs (queue `writeChan`)

    forever $ do
        -- Waits until an FileId has been inserted in the queue.
        fileId <- readChan queue

        mFile <- runDBIO $! get fileId
        when (isJust mFile) $ do
            -- Process the file if it still exists.
            let file = fromJust mFile
                hash = T.unpack $! fileSha1 file
                path = getPath (hashDir (uploadDir app) hash) Original

            -- Compress the file in a new temporary file.
            (tmpPath, tmpH) <- newTmpFile app "compression_"

            putStrLn "Compression:"
            tmpSize <- timeIt $ runResourceT $ do
                _ <- register $! hClose tmpH -- hClose two times has no effect.
                sourceFile path $= gzip $$ sinkHandle tmpH

                -- Computes the size of the new compressed file.
                liftIO $! fromIntegral <$> hFileSize tmpH

            _ <- liftIO $! printf "New size: %d - old: %d - gain: %d\n" tmpSize (fileSize file) (fileSize file - tmpSize)

            -- Replaces the file if the compressed one is smaller.
            if tmpSize < fileSize file
                then runDBIO $ do
                    -- Replaces the file during the transaction to ensures data
                    -- consistence.
                    update fileId [
                          FileCompressionQueue =. False
                        , FileCompressed =. Just tmpSize
                        ]
                    liftIO $! removeFile path
                    liftIO $! renameFile tmpPath path
                else removeFile tmpPath
  where
    runDBIO :: YesodPersistBackend App (ResourceT IO) a -> IO a
    runDBIO f = runResourceT $ runPool (persistConfig app) f (connPool app)

-- | Forks the compression daemon on a new thread and returns its 'ThreadId'.
forkCompressionDaemon :: App -> IO ThreadId
forkCompressionDaemon = forkIO . compressionDaemon
