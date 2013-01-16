{-# LANGUAGE OverloadedStrings #-}
module Model where

import Prelude
import Yesod

import Data.Int
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.Word

-- | File types recognized.
data FileType = Image | Audio | Video | Archive | UnknownType
    deriving (Show, Read, Eq)
derivePersistField "FileType"

type AdminKey = Int64
type Hmac = Text

-- | Used to give the type of a secondary image which can be displayed in the
-- browser.
data DisplayType = PNG | JPG | GIF
    deriving (Show, Read, Eq)
derivePersistField "DisplayType"

-- | Used to represents the different items which can be downloaded.
data ObjectType = Original
                | Miniature | Display DisplayType
                | WebMAudio | MP3
                | WebMVideo | MKV
                | CompressedFile Hmac
    deriving (Show, Read, Eq)
derivePersistField "ObjectType"

share [mkPersist sqlOnlySettings, mkMigrate "migrateAll"] [persistUpperCase|
LastAdminKey
    value AdminKey
    deriving Show

File
    sha1 Text
    type FileType
    size Word64
    compressionQueue Bool -- True if the file hasn't been compressed.
    compressed Word64 Maybe -- The compressed size if the file is compressed.
    uploaded UTCTime
    -- Each file is identified by its hash:
    UniqueFileSHA1 sha1
    deriving Show

Upload
    hmac Hmac -- An unique identifier of the upload generated from its ID.
    fileId FileId
    name Text
    description Text Maybe
    uploaded UTCTime
    hostname Text
    adminKey AdminKey
    views Word64 default=0
    lastView UTCTime
    bandwidth Word64 default=0
    UniqueUploadHmac hmac
    deriving Show

-- Saves the attributes of an image.
ImageAttrs
    fileId FileId
    width  Word32
    height Word32
    displayable DisplayType Maybe -- If Just, displays this object instead of
                                  -- the original image in the browser.
    UniqueImageAttrs fileId
    deriving Show

-- Saves EXIF tags from an image.
ExifTag
    fileId FileId
    title Text
    value Text
    UniqueExifTag fileId title
    deriving Show

-- Saves the attributes of a media.
MediaAttrs
    fileId FileId
    duration Word64 -- Duration in centisecond
    transcodeQueue Bool -- True if the file is planned to be transcoded.
    transcoded Bool -- True if the media has been re-encoded to be displayed
                    -- in the browser (HTML5 audio/video).
    UniqueMediaAttrs fileId
    deriving Show

-- Saves the attributes and tags of an MP3 track.
AudioAttrs
    fileId FileId
    album Text Maybe
    artist Text Maybe
    comment Text Maybe
    genre Text Maybe
    title Text Maybe
    track Int Maybe
    year Int Maybe
    lastFmUrl Text Maybe
    miniature Bool
    UniqueAudioAttrs fileId
    deriving Show

-- Saves the contained files of an archive.
ArchiveFile
    hmac Hmac -- An unique identifier of the file generated from its ID.
    fileId FileId
    path Text
    size Word64 Maybe -- Uncompressed size if not a directory
    UniqueArchiveFile fileId path
    UniqueArchiveFileHmac hmac
    deriving Show
|]
