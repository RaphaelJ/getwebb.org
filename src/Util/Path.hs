-- | Defines a few utilities to manages uploads files and directories.
module Util.Path (
      getFileSize, rootUploadDir, uploadDir, hashDir, hashDir', tmpDir
    , newTmpFile, getPath
    ) where

import Import

import Data.List
import qualified Data.Text as T
import System.IO
import System.FilePath

import Yesod.Default.Config

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

-- | Splits the hash of the file in four parts and constucts a four levels
-- directory path.
hashDir :: Text -> FilePath
hashDir = foldl' (</>) "" . map T.unpack . hashDir'

-- | Splits the hash of the file in four parts.
hashDir' :: Text -> [Text]
hashDir' hash =
    let (p1, hash') = T.splitAt 2 hash
        (p2, hash'') = T.splitAt 2 hash'
        (p3, p4) = T.splitAt 2 hash''
    in [p1, p2, p3, p4]

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
