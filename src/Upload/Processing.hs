{-# LANGUAGE OverloadedStrings #-}
-- | This module handles the processing of an uploaded file.
module Upload.Processing (
      UploadError (..), process, processFile, moveToTmp, hashFile, moveToUpload
    , computeHmac, toBase62
    ) where

import Import

import Control.Monad
import Network.Socket (NameInfoFlag (..), getNameInfo)
import System.Directory
import System.FilePath
import System.IO

import Control.Monad.Trans.Either
import Database.Persist.GenericSql (Single (..), rawSql)
import Database.Persist.Store (PersistValue (..))
import Data.Array.Unboxed (UArray, listArray, (!))
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Digest.Pure.SHA (sha1, hmacSha1, showDigest, integerDigest)
import Data.Digits (digits)
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime, addUTCTime)
import Network.Wai (remoteHost)

import Upload.Archive (processArchive)
import qualified Upload.Compression as C
import Upload.Image (processImage)
import Upload.Media (processMedia)
import Upload.Path (
      ObjectType (..), getFileSize, hashDir, uploadDir, newTmpFile, getPath
    )

import System.TimeIt (timeIt)

-- | Contains the diferents kinds of error that may occur during the processing
-- of an upload.
data UploadError = IPDailyLimitReached | FileTooLarge
    deriving (Show, Read)

-- | Process a list of files and return the list of the resulting database id
-- or the triggered error for each file.
process :: [FileInfo] -> Handler [Either UploadError Upload]
process fs = do
    admiKey <- getAdminKey
    forM fs (processFile admiKey)

-- | Process a file and returns its new ID from the database.
-- Returns either the uploaded file or an error message to be returned to the 
-- user.
processFile :: AdminKey -> FileInfo 
            -> Handler (Either UploadError Upload)
processFile adminKey f = do
    app <- getYesod
    extras <- getExtra
    clientHost <- remoteTextHost
    currentTime <- liftIO getCurrentTime
    let yesterday = (-3600 * 24) `addUTCTime` currentTime

    runEitherT $ do
        -- Checks the user limits before moving the file to fail as soon as 
        -- possible.
        allowed <- lift $ runDB $ checksIpLimits extras clientHost yesterday 0
        when (not allowed) $
            left IPDailyLimitReached

        tmpPath <- lift $ moveToTmp f
        size <- liftIO $ getFileSize tmpPath

        when (size > extraMaxFileSize extras) $ do
            liftIO $ removeFile tmpPath
            left FileTooLarge

        -- Checks if the file has been already uploaded by computing its hash.
        liftIO $ putStrLn "Hash file:"
        hash <- liftIO $ timeIt $ hashFile tmpPath
        let hashText = T.pack hash
            ext = T.pack $ takeExtension $ T.unpack $ fileName f

        let file = File {
              fileSha1 = hashText, fileType = UnknownType
            , fileSize = size, fileCompressed = Nothing
            , fileUploaded = currentTime
            }

        let path = getPath (hashDir (uploadDir app) hash) Original

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
                left IPDailyLimitReached

            mFileId <- lift $ insertUnique file
            (fileId, new) <- case mFileId of
                Just insertedFileId -> do
                    -- New file: moves the temporary file to its final
                    -- destination and adds the information to the database.
                    liftIO $ moveToUpload tmpPath path
                    liftIO $ putStrLn $ "New file " ++ path
                    return $! (insertedFileId, True)
                Nothing -> do
                    -- Existing file: remove the temporary file and
                    -- retrieves the FileId.
                    liftIO $ removeFile tmpPath
                    liftIO $ putStrLn "Existing file"
                    Just existingFile <- lift $ getBy (UniqueSHA1 hashText)
                    return $! (entityKey existingFile, False)

            let upload = Upload {
                  uploadHmac = "",  uploadFileId = fileId
                , uploadName = fileName f, uploadViews = 0
                , uploadUploaded = currentTime, uploadHost = clientHost
                , uploadLastView = currentTime, uploadAdminKey = adminKey
                }

            uploadId <- lift $ insert upload

            -- Computes and update the hmac of the upload.
            let hmac = computeHmac app uploadId
            lift $ update uploadId [UploadHmac =. hmac]

            return (upload { uploadHmac = hmac }, new)

        -- Process the special feature depending on the file type if it's a new
        -- file and puts it on the compressing queue afterward.
        let fileId = uploadFileId $ upload
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
        let sql = T.pack $ unlines [
                  "SELECT COUNT(*), COALESCE(SUM(f.size), 0)"
                , "FROM Upload AS u"
                , "INNER JOIN File AS f ON f.id = u.fileId"
                , "WHERE u.host = ? and u.uploaded >= ?;"
                ]
        [(Single n, Single size)] <- rawSql sql [
                  PersistText clientHost
                , PersistUTCTime minDate
                ]

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
hashFile :: FilePath -> IO String
hashFile path = do
    c <- B.readFile path
    return $! showDigest $ sha1 c

-- | Move a file to the given path.
-- Create the parent directories if they don't exist.
moveToUpload :: FilePath -> FilePath -> IO ()
moveToUpload path destPath = do
    createDirectoryIfMissing True (takeDirectory destPath)
    renameFile path destPath

-- | Returns the first height base 62 encoded digits of the upload hmac.
computeHmac :: App -> UploadId -> Text
computeHmac app uploadId =
    let key = encryptKey app
        PersistInt64 idInt = unKey uploadId
        hmac = integerDigest $ hmacSha1 key $ C.pack $ show idInt
    in T.pack $ take 8 $ toBase62 $ hmac

-- | Encodes an integer in base 62 (using letters and numbers).
toBase62 :: Integer -> String
toBase62 i =
    map (digitToChar !) $ digits 62 i
  where
    digitToChar :: UArray Integer Char
    digitToChar = listArray (0, 61) $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] 
