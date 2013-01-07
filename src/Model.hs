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
    UniqueSHA1 sha1
    deriving Show

Upload
    hmac Text -- An unique identifier of the upload generated from its ID.
    fileId FileId
    name Text
    mime Text
    uploaded UTCTime
    hostname Text
    adminKey AdminKey
    views Word64 default=0
    lastView UTCTime
    UniqueHmac hmac
    deriving Show

-- Saves the attributes of an image.
ImageAttrs
    fileId FileId
    width  Word32
    height Word32
    inBrowser Bool -- True if the image can be directly displayed in a browser.
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
    html5Encoded Bool -- True if the media has been re-encoded to be displayed.
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
    fileId FileId
    path Text
    size Word64 -- Uncompressed size
    UniqueArchiveFile fileId path
    deriving Show
|]
