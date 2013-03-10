{-# LANGUAGE OverloadedStrings #-}
module Model where

import Prelude
import Yesod

import Control.Monad.IO.Class
import Control.Monad.Trans.Writer
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Data.Word

import Database.Persist.GenericSql (Migration)
import Database.Persist.GenericSql.Raw (SqlPersist)

import Account.Foundation (Avatar (..))

-- | File types recognized.
data FileType = Image | Audio | Video | Archive | UnknownType
    deriving (Show, Read, Eq)
derivePersistField "FileType"

type Hmac = Text
-- | Types of resources which use an Hmac as an unique and global identifier.
data HmacResourceType = HmacFile | HmacUpload | HmacComment | HmacArchiveFile
    deriving (Show, Read, Eq)
derivePersistField "HmacResourceType"

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

data VoteType = Upvote | Downvote
    deriving (Show, Read, Eq)
derivePersistField "VoteType"

-- | Used to tags background jobs.
data JobType = Compression
             | Transcode
             | Resize DisplayType
             | ExifTags
    deriving (Show, Read, Eq)
derivePersistField "JobType"

share [mkPersist sqlOnlySettings, mkMigrate "migrateEnts"] [persistLowerCase|
User
    email Text
    name Text
    password Text -- SHA1 hash of the password.
    salt Text -- Salt used to hash the password.
    created UTCTime
    avatar AvatarId
    isAdmin Bool
    public Bool -- True if the user want new files to be public.
    UniqueUserEmail email
    UniqueUserName name
    deriving Show

AdminKey
    count Int
    user UserId Maybe
    deriving Show

-- Tracks HMACs allocations for uploads, archive files and comments.
UniqueHmac
    value Hmac -- An unique identifier generated from its ID.
    type HmacResourceType
    UniqueUniqueHmacValue value
    deriving Show

File
    hash Text -- SHA1 hash of the file
    type FileType
    size Word64
    compressed Word64 Maybe -- The compressed size if the file is compressed.
    created UTCTime
    count Int -- Number of uploads which point to this file
    -- Each file is identified by its hash:
    UniqueFileHash hash
    deriving Show

Upload
    hmac Hmac
    file FileId
    name Text
    description Text Maybe
    public Bool
    created UTCTime
    hostname Text
    adminKey AdminKeyId
    views Word64 default=0
    viewed UTCTime
    bandwidth Word64 default=0
    UniqueUploadHmac hmac
    deriving Show

Comment
    hmac Text
    user UserId
    upload UploadId
    message Text
    created UTCTime
    score Double
    upvotes Word64 default=0
    downvotes Word64 default=0
    UniqueCommentHmac hmac
    deriving Show

CommentVote
    comment CommentId
    user UserId
    type VoteType
    UniqueCommentVoteUser comment user
    deriving Show

-- Saves the attributes of an image.
ImageAttrs
    file FileId
    width  Word32
    height Word32
    displayable DisplayType Maybe -- If Just, displays this object instead of
                                  -- the original image in the browser.
    UniqueImageAttrs file
    deriving Show

-- Saves EXIF tags from an image.
ExifTag
    file FileId
    title Text
    value Text
    UniqueExifTag file title
    deriving Show

-- Saves the attributes of a media.
MediaAttrs
    file FileId
    duration Word64 -- Duration in centisecond.
    transcoded Bool -- True if the media has been re-encoded to be displayed
                    -- in the browser (HTML5 audio/video).
    UniqueMediaAttrs file
    deriving Show

-- Saves the attributes and tags of an MP3 track.
AudioAttrs
    file FileId
    album Text Maybe
    artist Text Maybe
    comment Text Maybe
    genre Text Maybe
    title Text Maybe
    track Int Maybe
    year Int Maybe
    lastFmUrl Text Maybe
    miniature Bool
    UniqueAudioAttrs file
    deriving Show

-- Saves the contained files of an archive.
ArchiveFile
    hmac Hmac -- An unique identifier of the file generated from its ID.
    file FileId
    path Text
    size Word64 Maybe -- Uncompressed size if not a directory.
    UniqueArchiveFile file path
    UniqueArchiveFileHmac hmac
    deriving Show

-- Saves the status of the background jobs queue.
Job
    file FileId
    type JobType
    created UTCTime
    completed Bool
    cpuTime Double Maybe -- CPU time in seconds used by the job to complete.
    exception Text Maybe -- The exception text if the process has failed.
    UniqueJob file type
    deriving Show

-- Each background job execution can depend on the termination of one or more
-- other jobs.
JobDependency
    job JobId
    dependency JobId
    UniqueJobDependency job dependency
    deriving Show
|]

-- | Creates each entities and their indexes.
migrateAll :: (MonadIO m, MonadBaseControl IO m) => Migration (SqlPersist m)
migrateAll = do
    migrateEnts
    lift $ tell [
          (False, T.concat [
                "CREATE INDEX IF NOT EXISTS \"", name, "\" ON ", cols
                ])
        | (name, cols) <- indexes
        ]
  where
    indexes = [
          ("upload_hostname", "\"upload\"(\"hostname\")")
        , ("upload_admin_key", "\"upload\"(\"admin_key\")")
        , ("upload_last_view", "\"upload\"(\"viewed\")")
        , ("comment_score", "\"comment\"(\"upload_id\", \"score\" DESC)")
        , ("job_completed", "\"job\"(\"completed\")")
        ]
