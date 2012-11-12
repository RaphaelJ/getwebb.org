module Upload.Processing (process, processFile, hashFile, hashPath)
    where

import Import

import Control.Monad
import Data.Word
import System.Directory
import System.FilePath
import System.IO

import qualified Data.ByteString.Lazy as LB
import Data.Conduit (($$), runResourceT)
import Data.Conduit.Lazy (lazyConsume)
import qualified Data.Conduit.List as CL
import Data.Digest.Pure.SHA (sha1, showDigest)
import Data.Text (pack, unpack)
import Data.Time.Clock (getCurrentTime)

import Upload.Utils (getAdminKey, hashPath)

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
    extras <- getExtra

    tmpPath <- moveToTmp f
    size <- fileSize tmpPath

    if size > extraMaxFileSize extras
        then return $! Left "File too large"
        else
            -- Checks if the file has been already uploaded.
            hash <- liftIO $ hashFile tmpPath
            let hashText = pack hash
            let hashDir = hashPath (extraUploadDir extras) hash

            fileId <- runDB $ do
                mFile <- getBy (UniqueSha1 hashText)
                case mFile of
                    Just f  ->
                        -- Existing file, remove the temporary file
                        liftIO $ removeFile tmpPath
                        return $! Right $ entityKey f
                    Nothing -> do
                        -- New file, move the temporary file to its final 
                        -- directory
                        currentTime <- liftIO getCurrentTime
                        let file = File hashText UnknownType size currentTime
                        liftIO $ moveToUpload tmpPath hashDir
                        Left $ insert file

            let path = 

            liftIO $ print hash
            liftIO $ print path

            return $! Left "Error"

-- | Reads the session value to get the admin key of the visitor. If the user
-- doesn\'t have a key, creates a new key.
adminKey :: Handler AdminKey
adminKey = do
    mKey <- getAdminKey

    -- Checks if the user has already an admin key.
    case mKey of
        Just k ->
            return k
        Nothing -> runDB $ do
            -- The user hasn't admin key. Takes the next free admin key.
            mLastK <- selectFirst [] []
            k <- case mLastK of
                Just (Entity keyId (LastAdminKey lastK)) -> do
                    -- Increments the last admin key to get a new value.
                    update keyId [LastAdminKeyValue +=. 1]
                    return $ lastK + 1
                Nothing -> do
                    -- Inserts the first admin key in the database.
                    _ <- insert (LastAdminKey 0)
                    return 0

            lift $ setSession sessionName (pack $ show k)
            return k

-- | Moves the uploaded file to a temporary file located in
-- @<upload dir>/tmp/@ and named with the @upload_@ prefix.
-- Returns the file\'s name and its size.
moveToTmp :: FileInfo -> Handler FilePath
moveToTmp f = do
    app <- getYesod
    (path, h) <- liftIO $ newTmpFile app "upload_"
    liftIO $ hClose h

    liftIO $ fileMove f path

    return path

-- | Returns the size in bytes of the given file.
fileSize :: FilePath -> Word64
fileSize path = withFile ReadMode hFileSize

-- | Computes the digest of a file.
hashFile :: FilePath -> IO String
hashFile path = do
    digest <- withFile path ReadMode $ \h -> do
        c <- LB.hGetContents h
        return $ sha1 c
    return $! showDigest digest

-- | Move a file to the upload directory given and name it @original@.
moveToUpload :: FilePath -> FilePath -> IO ()
moveToUpload path destDir = do
    
    renameFile 
