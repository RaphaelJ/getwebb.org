module Upload (process)
    where

import Import

import Data.Digest.Pure.SHA

type UploadId = Int
type FileId = Int

process :: [FileInfo] -> UploadId
process (f:fs) = undefined

processFile :: FileInfo -> FileId
processFile f = undefined