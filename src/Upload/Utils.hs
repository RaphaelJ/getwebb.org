-- | Defines a few utilities function which are used by the modules which
-- process the files upload.
module Upload.Utils (
      getAdminKey, adminKey, fileSize, hashPath, uploadDir, tmpDir, newTmpFile
    ) where

import Import hiding (fileSize)

import Data.Word
import System.IO
import System.FilePath

import Data.Text (pack, unpack)
import Yesod.Default.Config

-- | Reads the session value to get the admin key of the visitor. Returns
-- 'Norhing' if the user doesn\'t have a key.
getAdminKey :: Handler (Maybe AdminKey)
getAdminKey = do
    mKey <- lookupSession "admin_key"
    return $ (read . unpack) <$> mKey

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

            lift $ setSession "admin_key" (pack $ show k)
            return k

-- | Returns the size in bytes of the given file.
fileSize :: FilePath -> IO Word64
fileSize path = fromIntegral <$> withFile path ReadMode hFileSize

-- | Splits the hash in four parts and constucts a four level directory path in
-- the given directory.
hashPath :: FilePath -> String -> FilePath
hashPath dir hash =
    let (p1, hash') = splitAt 2 hash
        (p2, hash'') = splitAt 2 hash'
        (p3, p4) = splitAt 2 hash''
    in dir </> p1 </> p2 </> p3 </> p4

-- | Returns the directory where the uploaded files will be stored.
uploadDir :: App -> FilePath
uploadDir app = extraUploadDir $ appExtra $ settings app

-- | Returns the directory where the temporary files will be created.
tmpDir :: App -> FilePath
tmpDir app = uploadDir app </> "tmp"

-- | Opens a new temporary file with the given prefix.
newTmpFile :: App -> String -> IO (FilePath, Handle)
newTmpFile app prefix = openTempFile (tmpDir app) prefix
