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

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistUpperCase|
LastAdminKey
    value AdminKey
    deriving Show

Upload
    adminKey AdminKey
    date UTCTime
    deriving Show

File
    sha1 Text
    type FileType
    size Word64
    compressed Bool
    date UTCTime
    UniqueSha1 sha1
    deriving Show

UploadFile
    uploadId UploadId
    fileId FileId
    name Text
    ip Text
    views Int64 default=0
    lastView UTCTime
    deriving Show
|]