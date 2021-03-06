{-# LANGUAGE OverloadedStrings #-}
module Model (module Model, module Model.Field) where

import Prelude
import Yesod

import Control.Monad.Trans.Writer
import Data.Int
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (NominalDiffTime, UTCTime)
import Data.Word

import Database.Persist.Sql (SqlPersistT, Migration)

import Model.Field
import Util.Hmac.Type

-- Entities --------------------------------------------------------------------

share [mkPersist sqlOnlySettings, mkMigrate "migrateEnts"] [persistLowerCase|
User
    email                       Text
    name                        Text
    password                    Text -- SHA1 hash of the password.
    salt                        Text -- Salt used to hash the password.
    hostname                    Text -- IP address of the register host.
    created                     UTCTime
    avatar                      Int64
    admin                       Bool
    -- Links every upload of this user to this user.
    adminKey                    AdminKeyId
    commentsCount               Int default=0
    -- Settings :
    bio                         Textarea Maybe
    location                    Text Maybe
    website                     Text Maybe
    twitter                     Text Maybe
    defaultPublic               Bool -- True if wants new files to be public.

    UniqueUserEmail             email
    UniqueUserName              name
    UniqueUserAdminKey          adminKey
    deriving Show

AdminKey
    count                       Int
    deriving Show

-- Tracks HMACs allocations for uploads, archive files and comments.
UniqueHmac
    -- Unique identifier generated by hasing the UniqueHmacId.
    value                       Hmac
    type                        HmacResourceType

    UniqueUniqueHmacValue       value
    deriving Show

File
    hash                        Text -- SHA1 hash of the file
    type                        FileType
    size                        Word64
    -- The compressed size if the file is compressed.
    compressed                  Word64 Maybe
    created                     UTCTime
    count                       Int -- Number of uploads pointing to this file.

    -- Each file is identified by its hash :
    UniqueFileHash              hash
    deriving Show

Upload
    hmac                        Hmac
    file                        FileId
    name                        Text -- File's name.
    title                       Text -- Default: file's name.
    public                      Bool
    created                     UTCTime
    hostname                    Text -- IP address of the uploader.
    adminKey                    AdminKeyId
    score                       Double -- Computed from the number of views.
    views                       Word64 default=0
    viewed                      UTCTime
    bandwidth                   Word64 default=0 -- In bytes.
    commentsCount               Int default=0

    UniqueUploadHmac            hmac
    deriving Show

Comment
    hmac                        Hmac
    user                        UserId
    upload                      UploadId
    message                     Textarea
    hostname                    Text -- IP address of the author's host.
    created                     UTCTime
    score                       Double -- Computed from the votes.
    upvotes                     Int default=0
    downvotes                   Int default=0

    UniqueCommentHmac           hmac
    deriving Show

CommentVote
    comment                     CommentId
    user                        UserId
    type                        VoteType

    UniqueCommentVoteUser       comment user
    deriving Show

-- Saves the attributes of an image.
ImageAttrs
    file                        FileId
    width                       Word32
    height                      Word32

    -- If Just, provides the card on social medias instead of the source image.
    card                        ImageType Maybe

    -- If Just, displays this object instead of the original image in the
    -- browser.
    displayable                 ImageType Maybe

    UniqueImageAttrs            file
    deriving Show

-- Saves EXIF tags from an image.
ExifTag
    file                        FileId
    title                       Text
    value                       Text

    UniqueExifTag               file title
    deriving Show

-- Saves the attributes of a media.
MediaAttrs
    file                        FileId
    duration                    Word64 -- Duration in centisecond.
    -- True if the media has been re-encoded to be displayed in the browser 
    -- (HTML5 audio/video).
    transcoded                  Bool

    UniqueMediaAttrs            file
    deriving Show

-- Saves the attributes and tags of an MP3 track.
AudioAttrs
    file                        FileId
    album                       Text Maybe
    artist                      Text Maybe
    comment                     Text Maybe
    genre                       Text Maybe
    title                       Text Maybe
    track                       Int Maybe
    year                        Int Maybe
    lastFmUrl                   Text Maybe
    miniature                   Bool

    UniqueAudioAttrs            file
    deriving Show

-- Saves the contained files of an archive.
ArchiveFile
    hmac                        Hmac
    file                        FileId
    path                        Text
    -- Uncompressed size if not a directory.
    size                        Word64 Maybe

    UniqueArchiveFile           file path
    UniqueArchiveFileHmac       hmac
    deriving Show

-- Saves the status of the background jobs queue.
Job
    file                        FileId
    type                        JobType
    created                     UTCTime
    completed                   Bool
    -- CPU time in seconds used by the job to complete.
    cpuTime                     Double Maybe
    -- The exception text if the process has failed.
    exception                   Text Maybe

    UniqueJob                   file type
    deriving Show

-- Each background job execution can depend on the termination of one or more
-- other jobs.
JobDependency
    job                         JobId
    dependency                  JobId

    UniqueJobDependency         job dependency
    deriving Show
|]

-- Constraints -----------------------------------------------------------------

maxUploadTitleLength, maxCommentLength, maxUserBioLength, maxUserLocationLength,
    maxUserWebsiteLength :: Int

maxUploadTitleLength  = 250
maxCommentLength      = 400
maxUserBioLength      = 400
maxUserLocationLength = 100
maxUserWebsiteLength  = 250

-- | Seconds between two comments from the same user/host.
minCommentInterval :: NominalDiffTime
minCommentInterval = 60

-- Indexes ---------------------------------------------------------------------

-- | Creates each entities and their indexes.
migrateAll :: (MonadLogger m, MonadIO m, MonadBaseControl IO m) =>
              Migration (SqlPersistT m)
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
        , ("upload_admin_key_id", "\"upload\"(\"admin_key\", \"id\" DESC)")
        , ( "upload_admin_key_score_public"
          , "\"upload\"(\"admin_key\", \"score\" DESC, \"public\")")
        , ("upload_last_view", "\"upload\"(\"viewed\")")
        , ( "upload_score"
          , "\"upload\"(\"public\", \"score\" DESC, \"id\" DESC)")
        , ("user_hostname", "\"user\"(\"hostname\", \"created\")")
        , ("comment_user_created", "\"comment\"(\"user\", \"created\")")
        , ("comment_hostname_created", "\"comment\"(\"hostname\", \"created\")")
        , ("comment_score", "\"comment\"(\"upload\", \"score\" DESC)")
        , ( "commentvote_comment_user"
          , "\"comment_vote\"(\"comment\", \"user\")")
        , ("job_completed", "\"job\"(\"completed\")")
        ]
