-- | This module defines functions which remove files from the database and the
-- file repository.
module Upload.Remove (removeUpload)
    where

import Import

import Control.Monad
import qualified Data.Text as T
import System.Directory (removeDirectoryRecursive)

import Util.Path (hashDir)

removeUpload :: Entity Upload -> YesodDB App App ()
removeUpload (Entity uploadId upload) = do
    let fileId = uploadFileId upload
    delete uploadId

    -- Removes the corresponding file if it was the last upload.
    file <- updateGet fileId [FileCount -=. 1]
    when (fileCount file < 1) $ do
        -- Removes attributes
        case fileType file of
            Image   -> do
                deleteWhere [ImageAttrsFileId ==. fileId]
                deleteWhere [ExifTagFileId ==. fileId]
            Audio   -> do
                deleteWhere [MediaAttrsFileId ==. fileId]
                deleteWhere [AudioAttrsFileId ==. fileId]
            Video   ->
                deleteWhere [MediaAttrsFileId ==. fileId]
            Archive ->
                deleteWhere [ArchiveFileFileId ==. fileId]
            _       -> return ()

        -- Doesn't remove the jobs as they are kept as a log.

        delete fileId

        app <- lift $ getYesod
        let hash = T.unpack $ fileHash file
            dir = hashDir app hash
        liftIO $ removeDirectoryRecursive dir
        liftIO $ putStrLn "ok"
