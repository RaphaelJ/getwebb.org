-- | Defines a few utilities to process files during the processing of an
-- upload.
module Upload.Path (
      ObjectType (..)
    , getFileSize, hashDir, uploadDir, tmpDir, newTmpFile
    , getPath
    ) where

import Import

import Data.Word
import System.IO
import System.FilePath

import Yesod.Default.Config

-- | Used to represents the different items which can be downloaded.
data ObjectType = Original | Miniature | WebM | MKV | MP3 | PNG

-- | Returns the size in bytes of the given file.
getFileSize :: FilePath -> IO Word64
getFileSize path = fromIntegral <$> withFile path ReadMode hFileSize

-- | Splits the hash of the file in four parts and constucts a four level
-- directory path in the given directory.
hashDir :: FilePath -> String -> FilePath
hashDir dir hash =
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

-- | Returns the path to the given object in the upload directory.
getPath :: ObjectType -> FilePath -> FilePath
getPath Original  dir = dir </> "original"
getPath Miniature dir = dir </> "miniature" <.> "png"
getPath WebM      dir = dir </> "original" <.> "webm"
getPath MKV       dir = dir </> "original" <.> "mkv"
getPath MP3       dir = dir </> "original" <.> "mp3"
getPath PNG       dir = dir </> "original" <.> "png"
