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

    liftIO $! fileMove f path
    liftIO $! print hash
    liftIO $! print path

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
