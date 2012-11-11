module Model where

import Prelude
import Yesod

import Data.Int
import Data.Text (Text)
import Data.Time.LocalTime (TimeOfDay)

-- | Files types recognized.
data FileType = Image | Audio | Video | Archive | Unknown
    deriving (Show, Read, Eq)
derivePersistField "FileType"

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistUpperCase|
AdminKey
    deriving Show

Upload
    adminKeyID AdminKeyId
    uploadDate TimeOfDay default=CURRENT_TIMESTAMP
    deriving Show

File
    sha1 Text
    type FileType
    size Int64
    uploadDate TimeOfDay default=CURRENT_TIMESTAMP
    UniqueSha1 sha1
    deriving Show

UploadFile
    uploadId UploadId
    fileId FileId
    name Text
    ip Text
    uploadDate TimeOfDay default=CURRENT_TIMESTAMP
    views Int64 default=0
    lastView TimeOfDay default=CURRENT_TIMESTAMP
    deriving Show
|]
