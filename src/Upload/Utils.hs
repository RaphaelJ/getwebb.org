-- | Defines a few utilities function which are used by the modules which
-- process the files upload.
module Upload.Utils (
      getFileSize, hashPath, uploadDir, tmpDir, newTmpFile
    ) where

import Import

import Data.Word
import System.IO
import System.FilePath

import Data.Text (pack, unpack)
import Yesod.Default.Config


-- | Returns the size in bytes of the given file.
getFileSize :: FilePath -> IO Word64
getFileSize path = fromIntegral <$> withFile path ReadMode hFileSize

-- | Splits the hash of the file in four parts and constucts a four level directory path in
-- the given directory which is associated to the extension of the file.
hashPath :: FilePath -> File -> FilePath
hashPath dir hash extension =
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
