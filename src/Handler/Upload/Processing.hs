{-# LANGUAGE OverloadedStrings #-}
-- | Handles the whole processing of a recently uploaded file.
module Handler.Upload.Processing (
      UploadError (..), processFile, moveToTmp, hashFile, moveToUpload
    ) where

import Import

import Control.Monad
import Network.Socket (NameInfoFlag (..), getNameInfo)
import System.Directory
import System.FilePath
import System.IO

import Control.Monad.Trans.Either
import Database.Persist.Sql (Single (..), rawSql)
import qualified Data.ByteString.Lazy as B
import Data.Digest.Pure.SHA (sha1, showDigest)
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime, addUTCTime)
import Network.Wai (remoteHost)

import Handler.Upload.Archive (processArchive)
import Handler.Upload.Image (processImage)
import Handler.Upload.Media (processMedia)
import qualified JobsDaemon.Compression as C
import Util.API (ToAPIError (..))
import Util.Hmac (newHmac)
import Util.Path (ObjectType (..), getFileSize, uploadDir, newTmpFile, getPath)

import System.TimeIt (timeIt)

-- | Contains the diferents kinds of error that may occur during the processing
-- of an upload.
data UploadError = DailyIPLimitReached | FileTooLarge

instance ToAPIError UploadError where
    toAPIError DailyIPLimitReached = "Daily per IP limit reached."
    toAPIError FileTooLarge        = "The file exceeds the maximum file size."

-- | Process a file and returns its new ID from the database.
-- Returns either the uploaded file or an error message to be returned to the 
-- user.
processFile :: AdminKeyId -> FileInfo -> Bool
            -> Handler (Either UploadError Upload)
processFile adminKeyId f public = do
    app <- getYesod
    extras <- getExtra
    clientHost <- remoteTextHost
    time <- liftIO getCurrentTime
    let yesterday = (-3600 * 24) `addUTCTime` time

    runEitherT $ do
        -- Checks the user limits before moving the file to fail as soon as
        -- possible.
        allowed <- lift $ runDB $ checksIpLimits extras clientHost yesterday 0
        when (not allowed) $
            left DailyIPLimitReached

        tmpPath <- lift $ moveToTmp f
        size <- liftIO $ getFileSize tmpPath

        when (size > extraMaxFileSize extras) $ do
            liftIO $ removeFile tmpPath
            left FileTooLarge

        -- Checks if the file has been already uploaded by computing its hash.
        liftIO $ putStrLn "Hash file:"
        hash <- liftIO $ timeIt $ hashFile tmpPath
        let ext = T.toLower $ T.pack $ takeExtension $ T.unpack $ fileName f

        let path = getPath (uploadDir app hash) Original

        -- Checks if the file exists.
        -- eithFileId gets a Right value if its a new file which file needs
        -- to be processed.
        -- Inserts the file before knowing its type to lock and prevent others
        -- uploads to insert the same file during the processing.
        (upload, new) <- EitherT $ runDB $ runEitherT $ do
            -- Checks again if the user hasn't reach the upload limit.
            allowed' <- lift $ checksIpLimits extras clientHost yesterday size
            when (not allowed') $ do
                liftIO $ removeFile tmpPath
                left DailyIPLimitReached

            mFileId <- lift $ getBy $ UniqueFileHash hash
            (fileId, new) <- case mFileId of
                Just (Entity fileId _) -> do
                    -- Existing file: removes the temporary file and increments
                    -- the file's counter.
                    liftIO $ removeFile tmpPath
                    liftIO $ putStrLn "Existing file"
                    lift $ update fileId [FileCount +=. 1]
                    return (fileId, False)
                Nothing -> do
                    -- New file: moves the temporary file to its final
                    -- destination and adds the information to the database.
                    (key, _) <- lift $ newHmac HmacFile
                    let file = File {
                          fileHash = hash, fileType = UnknownType
                        , fileSize = size, fileCompressed = Nothing
                        , fileCreated = time, fileCount = 1
                        }
                    lift $ insertKey key file
                    liftIO $ moveToUpload tmpPath path
                    liftIO $ putStrLn $ "New file " ++ path
                    return (key, True)

            (key, hmac) <- lift $ newHmac HmacUpload
            let upload = Upload {
                  uploadHmac = hmac,  uploadFile = fileId
                , uploadName = fileName f, uploadDescription = Nothing
                , uploadPublic = public, uploadCreated = time
                , uploadHostname = clientHost, uploadAdminKey = adminKeyId
                , uploadViews = 0, uploadViewed = time
                , uploadBandwidth = 0, uploadCommentsCount = 0
                }

            lift $ insertKey key upload

            -- Increments the user's upload count
            lift $ update adminKeyId [AdminKeyCount +=. 1]

            return (upload, new)

        -- Processes the special feature depending on the file type if it's a
        -- new file and puts it on the compressing queue afterward.
        let fileId = uploadFile upload
        when new $
            lift (processImage path ext fileId)   .||.
            lift (processArchive path ext fileId) .||.
            lift (processMedia path ext fileId)   .||.
            lift (processUnknown app fileId)      >>
            return ()

        return upload
  where
    -- Checks if the client hasn't reach the upload limits since minDate.
    -- Returns True if the client is allowed to upload one more file.
    checksIpLimits extras clientHost minDate currentSize = do
        let sql = T.unlines [
                  "SELECT COUNT(*), COALESCE(SUM(f.size), 0)"
                , "FROM Upload AS u"
                , "INNER JOIN File AS f ON f.id = u.file"
                , "WHERE u.hostname = ? and u.created >= ?;"
                ]
        [(Single n, Single size)] <- rawSql sql [
                  PersistText clientHost
                , PersistUTCTime minDate
                ]

        let maxN    = extraMaxDailyUploads extras
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
        _ <- liftIO $ C.putFile app fileId []
        return True

    remoteTextHost = do
        addr <- remoteHost <$> waiRequest
        (Just host, _) <- liftIO $ getNameInfo [NI_NUMERICHOST] True False addr
        return $ T.pack $ host

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
hashFile :: FilePath -> IO Text
hashFile path = do
    c <- B.readFile path
    return $! T.pack $ showDigest $ sha1 c

-- | Move a file to the given path.
-- Create the parent directories if they don't exist.
moveToUpload :: FilePath -> FilePath -> IO ()
moveToUpload path destPath = do
    createDirectoryIfMissing True (takeDirectory destPath)
    renameFile path destPath
