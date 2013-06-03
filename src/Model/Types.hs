-- | Defines extended field types which are used in models.
module Model.Types where

import Prelude
import Yesod

import Data.Text (Text)
import qualified Data.Text as T

-- | File types recognized.
data FileType = Image | Audio | Video | Archive | UnknownType
    deriving (Show, Read, Eq)
derivePersistField "FileType"

type Hmac = Text
-- | Types of resources which use an Hmac as an unique and global identifier.
data HmacResourceType = HmacFile | HmacUpload | HmacComment | HmacArchiveFile
    deriving (Show, Read, Eq)
derivePersistField "HmacResourceType"

-- | Used to give the type of the secondary resized image which generated to
-- be displayed in the browser.
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
