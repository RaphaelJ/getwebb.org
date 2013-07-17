{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Extended field types which are used in models.
module Model.Field where

import Prelude

import Yesod

-- | File types recognized.
data FileType = Image | Audio | Video | Archive | UnknownType
    deriving (Show, Read, Eq)
derivePersistField "FileType"

-- | Types of resources which use an Hmac as an unique and global identifier.
data HmacResourceType = HmacFile | HmacUpload | HmacComment | HmacArchiveFile
    deriving (Show, Read, Eq)
derivePersistField "HmacResourceType"

-- | Used to identify the type of the secondary resized images which have been
-- generated to be displayed in the browser or in social medias.
data ImageType = PNG | JPG | GIF
    deriving (Show, Read, Eq)
derivePersistField "ImageType"

data ResizeJobType = ResizeCard | ResizeDisplay
    deriving (Show, Read, Eq)
derivePersistField "ResizeJobType"

-- | Used to tag background jobs.
data JobType = Compression
             | ExifTags
             | Resize ResizeJobType ImageType
             | Transcode
    deriving (Show, Read, Eq)
derivePersistField "JobType"

data VoteType = Upvote | Downvote
    deriving (Show, Read, Eq)
derivePersistField "VoteType"
