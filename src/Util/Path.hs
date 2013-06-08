-- | Few utilities to manages uploads file paths and directories.
module Util.Path (
      ObjectType (..)
    , getFileSize, rootUploadDir, uploadDir, hashDir, hashDir', tmpDir
    , newTmpFile, getPath
    ) where

import Import

import System.IO
import System.FilePath

import Yesod.Default.Config

import Util.Hmac (Hmac)
import Util.HashDir (hashDir, hashDir')

-- | Used to represents the different resources which can be downloaded.
data ObjectType = Original
                | Miniature | Display DisplayType
                | WebMAudio | MP3
                | WebMVideo | MKV
                | CompressedFile Hmac
    deriving (Show, Eq)

-- | Returns the size in bytes of the given file.
getFileSize :: FilePath -> IO Word64
getFileSize path = fromIntegral <$> withFile path ReadMode hFileSize

-- | Returns the directory where the uploaded files will be stored.
rootUploadDir :: App -> FilePath
rootUploadDir app = extraUploadDir $ appExtra $ settings app

-- | Splits the hash of the file in four parts and constucts a four levels
-- directory path in the upload directory.
uploadDir :: App -> Text -> FilePath
uploadDir app hash = rootUploadDir app </> hashDir hash

-- | Returns the directory where the temporary files will be created.
tmpDir :: App -> FilePath
tmpDir app = rootUploadDir app </> "tmp"

-- | Opens a new temporary file with the given prefix.
newTmpFile :: App -> String -> IO (FilePath, Handle)
newTmpFile app prefix = openTempFile (tmpDir app) prefix

-- | Returns the path to the given object in the upload directory.
getPath :: FilePath -> ObjectType -> FilePath
getPath dir Original           = dir </> "original"
getPath dir Miniature          = dir </> "miniature" <.> "png"
getPath dir (Display PNG)      = dir </> "original" <.> "png"
getPath dir (Display JPG)      = dir </> "original" <.> "jpg"
getPath dir (Display GIF)      = dir </> "original" <.> "gif"
getPath dir WebMAudio          = dir </> "original" <.> "webm"
getPath dir MP3                = dir </> "original" <.> "mp3"
getPath dir WebMVideo          = dir </> "original" <.> "webm"
getPath dir MKV                = dir </> "original" <.> "mkv"
getPath dir (CompressedFile _) = getPath dir Original
