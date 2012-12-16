module Upload.Processing (process, processFile)
    where

import Import

import Control.Monad
import System.Directory
import System.FilePath
import System.IO

import qualified Data.ByteString.Lazy as LB
import Data.Digest.Pure.SHA (sha1, showDigest)
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)

import Upload.Archive (processArchive)
import Upload.Image (processImage)
import Upload.Media (processMedia)
import qualified Upload.Compression as C
import Upload.Utils (getFileSize, hashDir, uploadDir, uploadFile, newTmpFile)

import System.TimeIt (timeIt)

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
            liftIO $ putStrLn "Hash file:"
            hash <- liftIO $ timeIt $ hashFile tmpPath
            let hashText = T.pack hash
                ext = T.pack $ takeExtension $ T.unpack $ fileName f

            currentTime <- liftIO getCurrentTime
            let file = File {
                  fileSha1 = hashText, fileExtension = ext
                , fileType = UnknownType, fileSize = size
                , fileCompressed = Nothing, fileDate = currentTime
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
                Right fileId ->
                    processImage path ext fileId   .||.
                    processArchive path ext fileId .||.
                    processMedia path ext fileId   >>
                    liftIO (app `C.putFile` fileId)
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
