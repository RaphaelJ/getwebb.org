{-# LANGUAGE OverloadedStrings #-}
-- | This module handles the processing of an uploaded file.
module Upload.Processing (processFile, moveToTmp, hashFile, moveToUpload)
    where

import Import

import Control.Monad
import System.Directory
import System.FilePath
import System.IO

import qualified Data.ByteString.Lazy as LB
import Data.Digest.Pure.SHA (sha1, showDigest)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime (..), getCurrentTime, addUTCTime)

import Upload.Archive (processArchive)
import Upload.Image (processImage)
import Upload.Media (processMedia)
import qualified Upload.Compression as C
import Upload.Utils (getFileSize, hashDir, uploadDir, uploadFile, newTmpFile)

import System.TimeIt (timeIt)

-- | Process a file and returns its new ID from the database.
-- Returns either the uploaded file ID in the DB or an error message to be
-- returned to the user.
processFile :: FileInfo -> Handler (Either Text UploadId)
processFile f = do
    app <- getYesod
    extras <- getExtra
    clientIp <- (T.pack . show . remoteHost) <$> waiRequest
    currentTime <- liftIO getCurrentTime

    -- Checks the user limits to fail as soon as possible.
    let yesterday = (-3600 * 24) `addUTCTime` currentTime
    runDB $ checksIpLimits extras clientIp yesterday

    tmpPath <- moveToTmp f
    size <- liftIO $ getFileSize tmpPath

    if size > extraMaxFileSize extras
        then return $! Left "File too large"
        else do
            -- Checks if the file has been already uploaded by computing his
            -- hash.
            liftIO $ putStrLn "Hash file:"
            hash <- liftIO $ timeIt $ hashFile tmpPath
            let hashText = T.pack hash
                ext = T.pack $ takeExtension $ T.unpack $ fileName f

            let file = File {
                  fileSha1 = hashText, fileType = UnknownType
                , fileSize = size, fileCompressed = Nothing
                , fileUpload = currentTime
                }

            let path = uploadFile (hashDir (uploadDir app) hash)

            -- Checks if the file exists.
            -- eithFileId gets a Right value if its a new file which file needs
            -- to be processed.
            -- Inserts the file before knowing its type to lock others uploads
            -- to insert the same file during the processing.
            eithFileId <- runDB $ do
                mFileId <- insertUnique file
                case mFileId of
                    Just inseredFileId -> do
                        -- New file: moves the temporary file to its final
                        -- destination and adds the information to the database.
                        liftIO $ moveToUpload tmpPath path
                        liftIO $ putStrLn $ "New file " ++ path
                        return $! Right inseredFileId
                    Nothing -> do
                        -- Existing file: remove the temporary file and
                        -- retrieves the FileId.
                        liftIO $ removeFile tmpPath
                        liftIO $ putStrLn "Existing file"
                        Just existingFile <- getBy (UniqueSHA1 hashText)
                        return $! Left $ entityKey existingFile

            -- Process the special feature depending on the file type if it's a
            -- new file and puts it on the compressing queue afterward.
            case eithFileId of
                Right fileId -> do
                    _ <- processImage path ext fileId   .||.
                         processArchive path ext fileId .||.
                         processMedia path ext fileId   .||.
                         processUnknown app fileId
                    
                _ ->
                    return ()

            return $! Right 
  where
    -- Checks if the client hasn't reach the upload limits since minDate.
    -- Returns True if the client is allowed to upload one more file.
    checksIpLimits clientIp extras minDate currentSize =
        (n, size) <- rawSql "SELECT COUNT(*), SUM(f.size)            \
                            \FROM Upload AS u                        \
                            \INNER JOIN File AS f ON f.id = u.fileId \
                            \WHERE u.ip = ? and date >= ?;"
                            [PersistText clientIp, PersistUTCTime minDate]

        let maxN = extraMaxDailyUploads extras
            maxSize = extraMaxDailySize extras
        return $ maxN > n && maxSize > size + currentSize

    -- Runs the first action, runs the second if the first returned 'False'.
    -- Returns a || b.
    (.||.) :: Monad m => m Bool -> m Bool -> m Bool
    infixr 2 .||.
    a .||. b = do
        retA <- a
        if retA then return True
                else b

    processUnknown app fileId = do
        liftIO $ app `C.putFile` fileId
        return True

-- | Moves the uploaded file to a temporary file with a @upload_@ prefix.
-- Returns the temporary file name.
moveToTmp :: FileInfo -> Handler FilePath
moveToTmp f = do
    app <- getYesod
    (path, h) <- liftIO $ newTmpFile app "upload_"
    liftIO $ hClose h

    liftIO $ putStrLn "Move file:"
    liftIO $ timeIt $ fileMove f path

    return path

-- | Computes the digest of a file.
hashFile :: FilePath -> IO String
hashFile path = do
    c <- LB.readFile path
    return $! showDigest $ sha1 c

-- | Move a file to the given path.
-- Create the parent directories if they don't exist.
moveToUpload :: FilePath -> FilePath -> IO ()
moveToUpload path destPath = do
    createDirectoryIfMissing True (takeDirectory destPath)
    renameFile path destPath
