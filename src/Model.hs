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

Upload
    adminKey AdminKey
    date UTCTime
    ip Text
    deriving Show

File
    sha1 Text
    extension Text
    type FileType
    size Word64
    compressed Word64 Maybe -- The compressed size if the file is compressed.
    date UTCTime
    -- Each file is identified by its hash and extension:
    UniqueFiles sha1 extension
    deriving Show

UploadFile
    uploadId UploadId
    fileId FileId
    name Text
    views Int64 default=0
    lastView UTCTime
    deriving Show

-- Saves EXIF tags from an image.
ExifTag
    fileId FileId
    title Text
    value Text

-- Saves the attributes of an image.
ImageAttrs
    fileId FileId
    width  Word32
    height Word32

-- Saves the contained files of an archive.
ArchiveFile
    fileId FileId
    path Text
    size Word64 -- Uncompressed size

-- Saves the attributes of a media.
MediaAttrs
    fileId FileId
    duration Word64 -- Duration in centisecond

-- Saves the attributes of an MP3 track.
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
|]
