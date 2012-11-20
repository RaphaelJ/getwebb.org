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
    ip Text
    deriving Show

File
    sha1 Text
    extension Text
    type FileType
    size Word64
    compressed Word64 Maybe -- The compressed size if the file is compressed.
    date UTCTime

    -- Each file is identified by its hash and extension
    UniqueFiles sha1 extension
    deriving Show

UploadFile
    uploadId UploadId
    fileId FileId
    name Text
    views Int64 default=0
    lastView UTCTime
    deriving Show
|]
