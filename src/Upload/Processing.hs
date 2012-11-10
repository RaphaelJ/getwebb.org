module Upload.Processing (process, processFile, hashFile, hashPath)
    where

import Import

import Control.Monad
import System.FilePath

import Data.ByteString.Lazy (fromChunks)
import Data.Conduit (runResourceT)
import Data.Conduit.Lazy (lazyConsume)
import Data.Digest.Pure.SHA (sha1, showDigest)


type UploadId = Int
type FileId = Int

-- | Process a upload using a list of files. Returns the ID of the new upload
-- row in the database.
process :: [FileInfo] -> Handler UploadId
process fs = do
    forM_ fs processFile
    return 1

-- | Process a file and returns its new ID from the database.
processFile :: FileInfo -> Handler FileId
processFile f = do
    hash <- liftIO $! hashFile f
    liftIO $! print hash
    liftIO $! print $! hashPath hash
    return 1

hashFile :: FileInfo -> IO String
hashFile f =
    runResourceT $! do
        bs <- lazyConsume $! fileSource f
        return $! showDigest $! sha1 $! fromChunks bs

-- | Splits the hash in four parts and constucts a four level directory path.
hashPath :: String -> FilePath
hashPath hash =
    let (p1, hash') = splitAt 2 hash
        (p2, hash'') = splitAt 2 hash'
        (p3, p4) = splitAt 2 hash''
    in p1 </> p2 </> p3 </> p4
