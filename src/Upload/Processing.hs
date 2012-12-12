module Upload.Processing (process, processFile, hashFile, hashPath)
    where

import Import

import Control.Monad
import System.Directory
import System.FilePath
import System.IO

import qualified Data.ByteString.Lazy as LB
import Data.Digest.Pure.SHA (sha1, showDigest)
import Data.Text (pack)
import Data.Time.Clock (getCurrentTime)

import Upload.Compression (putFile)
import Upload.Utils (getFileSize, hashPath, uploadDir, newTmpFile)

-- | Process a upload using a list of files. Returns the ID of the new upload
-- row in the database.
process :: [FileInfo] -> Handler ()
process fs = do
    adminKey <- getAdminKey
    liftIO $ print adminKey

    forM_ fs processFile

-- | Process a file and returns its new ID from the database.
-- Returns either the uploaded file ID in the DB or an error message to be
-- returned to the user.
processFile :: FileInfo -> Handler (Either Text UploadFileId)
processFile f = do
    app <- getYesod
    extras <- getExtra

    tmpPath <- moveToTmp f
    size <- liftIO $ getFileSize tmpPath

    if size > extraMaxFileSize extras
        then return $! Left "File too large"
        else do
            -- Checks if the file has been already uploaded by computing his
            -- hash.
            hash <- liftIO $ hashFile tmpPath
            let hash = pack hash
            let ext = pack $ takeExtension $ fileName f

            currentTime <- liftIO getCurrentTime
            let file = File {
                  fileSha1 = hashText, fileExtension = ext
                , fileType = UnknownType, fileSize = size
                , fileCompressed = Nothing, fileDate = currentTime
                }

            let hashDir = hashPath (uploadDir app) file
                path = hashDir </> "original"

            -- Checks if the file exists.
            -- eithFileId gets a Right value if its a new file which file needs
            -- to be processed.
            -- Inserts the file before knowing its type to lock others uploads
            -- to insert the same file during the processing.
            eithFileId <- runDB do
                mFileId <- insertUnique file
                case mFileId of
                    Just inseredFileId -> do
                        -- New file: moves the temporary file to its final
                        -- destination and adds the information to the database.
                        liftIO $ moveToUpload tmpPath hashDir
                        liftIO $ print "New file"
                        return $ Right inseredFileId
                    Nothing -> do
                        -- Existing file: remove the temporary file and
                        -- retrieves the FileId.
                        liftIO $ removeFile tmpPath
                        liftIO $ print "Existing file"
                        Just existingFile <- getBy (UniqueSha1 hashText ext)
                        return $ Left $ entityKey existingFile

            -- Process the special feature depending on the file type if it's a
            -- new file.
            case eithFileId of
                Right fileId ->
                    processImage hashDir path ext fileId   .||.
                    processArchive hashDir path ext fileId .||.
                    processMedia hashDir path ext fileId   >>
                    liftIO $ app `putFile` fileId
                _ ->
                    return ()

            return $! Left "Error"
  where
    -- | Runs the first action, runs the second if the first returned 'False'.
    -- Returns a || b.
    (.||.) :: Monad m => m Bool -> m Bool -> m Bool
    infixr 2 .||.
    a .||. b = do
        retA <- a
        if retA then return True
                else b

-- | Moves the uploaded file to a temporary file with a @upload_@ prefix.
-- Returns the file\'s name.
moveToTmp :: FileInfo -> Handler FilePath
moveToTmp f = do
    app <- getYesod
    (path, h) <- liftIO $ newTmpFile app "upload_"
    liftIO $ hClose h

    liftIO $ fileMove f path

    return path

-- | Computes the digest of a file.
hashFile :: FilePath -> IO String
hashFile path = do
    c <- LB.readFile path
    return $! showDigest $ sha1 c

-- | Move a file to the upload directory given and name it @original@.
-- Create the parent directories if they don't exist.
moveToUpload :: FilePath -> FilePath -> IO ()
moveToUpload path destDir = do
    createDirectoryIfMissing True destDir
    renameFile path (destDir </> "original")
