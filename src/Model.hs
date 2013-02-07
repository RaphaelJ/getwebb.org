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

import Database.Persist.GenericSql.Raw (SqlPersist)

type Hmac = Text

-- | File types recognized.
data FileType = Image | Audio | Video | Archive | UnknownType
    deriving (Show, Read, Eq)
derivePersistField "FileType"

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

-- | Used to tags background jobs.
data JobType = Compression
             | Transcode
             | Resize DisplayType
             | ExifTags
    deriving (Show, Read, Eq)
derivePersistField "JobType"

share [mkPersist sqlOnlySettings, mkMigrate "migrateEnts"] [persistLowerCase|
AdminKey
    count Int
    deriving Show

File
    hash Text -- SHA1 hash of the file
    type FileType
    size Word64
    compressed Word64 Maybe -- The compressed size if the file is compressed.
    uploaded UTCTime
    count Int -- Number of uploads which point to this file
    -- Each file is identified by its hash:
    UniqueFileHash hash
    deriving Show

Upload
    hmac Hmac -- An unique identifier of the upload generated from its ID.
    fileId FileId
    name Text
    description Text Maybe
    uploaded UTCTime
    hostname Text
    adminKey AdminKeyId
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
    duration Word64 -- Duration in centisecond.
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
    size Word64 Maybe -- Uncompressed size if not a directory.
    UniqueArchiveFile fileId path
    UniqueArchiveFileHmac hmac
    deriving Show

-- Saves the status of the background jobs queue.
Job
    fileId FileId
    type JobType
    completed Bool
    cpuTime Double Maybe -- CPU time in seconds used by the job to complete.
    exception Text Maybe -- The exception text if the process has failed.
    UniqueJob fileId type

-- Each background job execution can depend on the termination of one or more
-- other jobs.
JobDependency
    jobId JobId
    dependency JobId
    UniqueJobDependency jobId dependency
|]

-- | Creates each entities and their indexes.
migrateAll :: (MonadIO m, MonadBaseControl IO m) =>
              WriterT [Text] (WriterT [(Bool, Text)] (SqlPersist m)) ()
migrateAll = do
    migrateEnts
    lift $ tell [
          (False, T.concat [
                "CREATE INDEX IF NOT EXISTS\"", name, "\" ON ", cols
                ])
        | (name, cols) <- indexes
        ]
  where
    indexes = [
          ("upload_hostname", "\"upload\"(\"hostname\")")
        , ("upload_admin_key", "\"upload\"(\"admin_key\")")
        , ("upload_last_view", "\"upload\"(\"last_view\")")
        , ("job_completed", "\"job\"(\"completed\")")
        ]
