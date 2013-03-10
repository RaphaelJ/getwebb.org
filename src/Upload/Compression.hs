-- | This module declares a daemon which will receive file to compress on a
-- concurrent queue. Try to compress the file using the GZip algorithm and
-- update the database entry if the compressed file is smaller than the original
-- file.
module Upload.Compression (
    -- * Compression
      compressFile
    -- * Background compression queue management
    , putFile
    ) where

import Import

import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import System.Directory (renameFile, removeFile)
import System.IO (IOMode (..), openFile, hFileSize, hClose)

import Codec.Compression.GZip (
      compressWith, defaultCompressParams, compressLevel, bestCompression
    )

import JobsDaemon (registerJob, runDBIO)
import Util.Path (hashDir, newTmpFile, getPath)

-- | Tries to compress a file. Replaces the original file and updates the
-- database if the compressed file is smaller.
compressFile :: App -> FileId -> IO ()
compressFile app fileId = do
    Just file <- runDBIO app $ get fileId

    let path = getPath (hashDir app (fileHash file)) Original

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

-- | Adds a file to a background compression queue.
putFile :: App -> FileId -> [JobId] -> IO JobId
putFile app fileId deps =
    registerJob app fileId Compression deps (compressFile app fileId)
