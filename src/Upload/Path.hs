-- | Defines a few utilities to process files during the processing of an
-- upload.
module Upload.Path (
      getFileSize, hashDir, uploadDir, tmpDir, newTmpFile, getPath, computeHmac
    , toBase62
    ) where

import Import

import Data.Array.Unboxed (UArray, listArray, (!))
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Digest.Pure.SHA (hmacSha1, integerDigest)
import Data.Digits (digits)
import qualified Data.Text as T
import System.IO
import System.FilePath

import Yesod.Default.Config
import Database.Persist.Store (PersistValue (..))

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

-- | Returns the first eight base 62 encoded digits of the key hmac.
computeHmac :: App -> Key val -> Hmac
computeHmac app idKey =
    let key = encryptKey app
        PersistInt64 idInt = unKey idKey
        hmac = integerDigest $ hmacSha1 key $ C.pack $ show idInt
    in T.pack $ take 8 $ toBase62 $ hmac

-- | Encodes an integer in base 62 (using letters and numbers).
toBase62 :: Integer -> String
toBase62 i =
    map (digitToChar !) $ digits 62 i
  where
    digitToChar :: UArray Integer Char
    digitToChar = listArray (0, 61) $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] 
