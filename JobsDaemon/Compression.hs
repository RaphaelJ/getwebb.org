-- | Uses the JobDaemon to try to compress recently uploaded files using the
-- GZip algorithm and update the database entry if the compressed file is
-- smaller than the original file.
module JobsDaemon.Compression (
    -- * Job
      jobCompress
    -- * Background compression queue management
    , putFile
    ) where

import Import

import qualified Data.ByteString.Lazy as L
import System.Directory (renameFile, removeFile)
import System.IO (IOMode (..), openFile, hFileSize, hClose)

import Codec.Compression.GZip (
      compressWith, defaultCompressParams, compressLevel, bestCompression
    )

import JobsDaemon.Util (registerJob, runDBIO)
import Util.Path (ObjectType (..), uploadDir, newTmpFile, getPath)

-- | Tries to compress a file. Replaces the original file and updates the
-- database if the compressed file is smaller.
jobCompress :: App -> FileId -> IO ()
jobCompress app fileId = do
    Just file <- runDBIO app $ get fileId

    let path = getPath (uploadDir app (fileHash file)) Original

    h <- liftIO $ openFile path ReadMode

    -- Compresses the file in a new temporary file.
    (tmpPath, tmpH) <- newTmpFile app "compression_"

    L.hGetContents h >>= L.hPut tmpH . compressWith params
    hClose h

    -- Computes the size of the new compressed file.
    tmpSize <- word64 <$> hFileSize tmpH
    hClose tmpH

    -- Replaces the file if the compressed one is smaller.
    -- Replaces and removes the file during the transaction to ensures data
    -- consistence.
    if tmpSize < fileSize file
        then do
            runDBIO app $ do
                -- Checks if the file still exists.
                exists <- get fileId
                case exists of
                    Just _ -> do
                        update fileId [FileCompressed =. Just tmpSize]
                        liftIO $ removeFile path
                        liftIO $ renameFile tmpPath path
                    Nothing -> do
                        -- The original file has been removed.
                        liftIO $ removeFile tmpPath
        else do
            removeFile tmpPath
  where
    -- Compression parameters.
    params = defaultCompressParams { compressLevel = bestCompression }

-- | Adds a file to the background compression queue.
putFile :: App -> FileId -> [JobId] -> IO JobId
putFile app fileId deps = registerJob app fileId Compression deps
                                      (jobCompress app fileId)
