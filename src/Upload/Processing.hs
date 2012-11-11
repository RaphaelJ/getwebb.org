module Upload.Processing (process, processFile, hashFile, hashPath)
    where

import Import

import Control.Monad
import System.FilePath

import Data.ByteString.Lazy (fromChunks)
import Data.Conduit (runResourceT)
import Data.Conduit.Lazy (lazyConsume)
import Data.Digest.Pure.SHA (sha1, showDigest)

-- | Process a upload using a list of files. Returns the ID of the new upload
-- row in the database.
process :: [FileInfo] -> Handler ()
process fs = do
    forM_ fs processFile

-- | Process a file and returns its new ID from the database.
processFile :: FileInfo -> Handler ()
processFile f = do
    extras <- getExtra

    hash <- liftIO $! hashFile f
    let path = hashPath (extraUploadDir extras) hash
    
    adminKey <- getAdminKey
    liftIO $ print adminKey

    liftIO $ fileMove f path

    liftIO $ print hash
    liftIO $ print path

-- | Reads the session value to get the admin key of the visitor. If the user
-- doesn\'t have a key, creates a new key.
getAdminKey :: Handler AdminKey
getAdminKey = do
    let sessionName = "admin_key"
    mKey <- lookupSession sessionName

    -- Checks if the user has already an admin key.
    case mKey of
        Just k ->
            return $ read $ unpack k
        Nothing -> runDB $ do
            -- The user hasn't admin key. Takes the next free admin key.
            mLastK <- selectFirst [] []
            k <- case mLastK of
                    Just (Entity rowId (LastAdminKey lastK)) -> do
                        -- Increments the last admin key to get a new value.
                        let newK = lastK + 1
                        update rowId [LastKey newK]
                        return newK
                    Nothing -> do
                        -- Inserts the first admin key in the database.
                        _ <- insert (LastAdminKey 0)
                        return 0

            lift $ setSession sessionName (pack $ show k)
            return k

-- | Computes the digest of a uploaded file.
hashFile :: FileInfo -> IO String
hashFile f =
    runResourceT $! do
        bs <- lazyConsume $! fileSource f
        return $! showDigest $! sha1 $! fromChunks bs

-- | Splits the hash in four parts and constucts a four level directory path in
-- the given directory.
hashPath :: FilePath -> String -> FilePath
hashPath dir hash =
    let (p1, hash') = splitAt 2 hash
        (p2, hash'') = splitAt 2 hash'
        (p3, p4) = splitAt 2 hash''
    in dir </> p1 </> p2 </> p3 </> p4
