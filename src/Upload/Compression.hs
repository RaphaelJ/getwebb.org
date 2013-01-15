-- | This module declares a daemon which will receive file to compress on a
-- concurrent queue. Try to compress the file using the GZip algorithm and
-- update the database entry if the compressed file is smaller than the original
-- file.
module Upload.Compression (
    -- * Daemon processing queue management
      putFile, restoreQueue
    -- * Compression
    , compressFile
    ) where

import Import

import Control.Monad
import Data.Maybe
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import System.Directory
import System.IO (IOMode (..), openFile, hFileSize, hClose)

import Codec.Compression.GZip (
      compressWith, defaultCompressParams, compressLevel, bestCompression
    )
import Database.Persist.Query.Internal (selectKeysList)

import JobsDaemon (putJob, runDBIO)
import Upload.Path (hashDir, uploadDir, newTmpFile, getPath)

import System.TimeIt (timeIt)
import Text.Printf

-- | Adds a file to a background compression queue.
putFile :: App -> FileId -> IO ()
putFile app = putJob app . compressFile app

-- | Reload the previous state of the compression queue from the database and
-- put them on the background queue.
restoreQueue :: App -> IO ()
restoreQueue app = do
    fs <- runDBIO app $ selectKeysList [FileCompressionQueue ==. True] 
                                       [Asc FileId]
    forM_ fs (app `putFile`)

-- | Tries to compress a file. Replaces the original file and updates the
-- database if the compressed file is smaller.
compressFile :: App -> FileId -> IO ()
compressFile app fileId = do
    -- Retrieves the file from the database to ensures it still exists.
    mFileInfo <- runDBIO app $ do
        mFile <- get fileId

        case mFile of
            Just file -> do
                let hash = T.unpack $! fileSha1 file
                    path = getPath (hashDir (uploadDir app) hash) Original
                h <- liftIO $ openFile path ReadMode
                return $! Just (path, h, fileSize file)
            Nothing -> return Nothing -- File removed

    when (isJust mFileInfo) $ do
        let Just (path, h, size) = mFileInfo

        -- Compress the file in a new temporary file.
        (tmpPath, tmpH) <- newTmpFile app "compression_"

        putStrLn "Compression:"
        timeIt $ L.hGetContents h >>= L.hPut tmpH . compressWith params
        hClose h

        -- Computes the size of the new compressed file.
        tmpSize <- word64 <$> hFileSize tmpH
        hClose tmpH

        _ <- liftIO $! printf "New size: %d - old: %d - gain: %d\n" tmpSize size (size - tmpSize)

        -- Replaces the file if the compressed one is smaller.
        -- Replaces and removes the file during the transaction to ensures data
        -- consistence.
        runDBIO app $ if tmpSize < size
            then do
                -- Checks if the file still exists.
                exists <- get fileId
                case exists of
                    Just _ -> do

                        update fileId [
                            FileCompressionQueue =. False
                            , FileCompressed =. Just tmpSize
                            ]
                        liftIO $ removeFile path
                        liftIO $ renameFile tmpPath path
                    Nothing -> do
                        -- The original file has been removed.
                        liftIO $ removeFile tmpPath
            else do
                update fileId [FileCompressionQueue =. False]
                liftIO $ removeFile tmpPath
  where
    -- Compression parameters.
    params = defaultCompressParams { compressLevel = bestCompression }
