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
import Control.Concurrent (ThreadId, Chan, forkIO, newChan, writeChan, readChan)
import Control.Monad
import System.Directory
import System.FilePath
import System.IO

import Database.Persist.Store (runPool)
import Data.Conduit (($$), ($=), runResourceT)
import Data.Conduit.Binary (sourceFile, sinkHandle)
import Data.Conduit.Zlib (gzip)
import Data.Text (unpack)

import Upload.Utils (hashPath, uploadDir, newTmpFile)

type CompressionQueue = Chan FileId

-- | Waits files to compress on the concurrent queue and process them. Never
-- returns.
compressionDaemon :: App -> IO ()
compressionDaemon app =
    forever $ do
        -- Waits until an FileId has been insered in the queue.
        fileId <- readChan $ compressionQueue app

        mFile <- runDBIO $ get fileId
        when (isJust mFile) $ do
            -- Process the file if it still exists.
            let file = fromJust mFile
            let hash = unpack $ fileSha1 file
            let path = hashPath (uploadDir app) hash </> "original"

            -- Compress the file in a new temporary file.
            (tmpPath, tmpH) <- newTmpFile app "compression_"
            runResourceT $ sourceFile path $= gzip $$ sinkHandle tmpH

            -- Computes the size of both files.
            tmpSize <- fromIntegral <$> hFileSize tmpH
            hClose tmpH

            -- Remplaces the file if the compressed one is smaller.
            liftIO $ print (tmpSize, fileSize file)

            if tmpSize < fileSize file
                then runDBIO $ do
                    update fileId [FileCompressed =. Just tmpSize]
                    liftIO $ removeFile path
                    liftIO $ renameFile tmpPath path
                else removeFile tmpPath
  where
    runDBIO :: YesodPersistBackend App IO a -> IO a
    runDBIO f = runPool (persistConfig app) f (connPool app)

-- | Forks the compression daemon on a new thread and returns its 'ThreadId'.
forkCompressionDaemon :: App -> IO ThreadId
forkCompressionDaemon = forkIO . compressionDaemon

-- | Initialises a new compression queue to be insered in the foundation type.
newQueue :: IO CompressionQueue
newQueue = newChan

-- | Adds a file to the compression queue.
putFile :: App -> FileId -> IO ()
putFile = writeChan . compressionQueue