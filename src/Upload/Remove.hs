-- | This module defines functions which remove files from the database and the
-- file repository.
module Upload.Remove (removeUpload)
    where

import Import

import Control.Monad
import System.Directory (removeDirectoryRecursive)

import Util.Path (uploadDir)

removeUpload :: Entity Upload -> YesodDB App App ()
removeUpload (Entity uploadId upload) = do
    let fileId = uploadFile upload
    delete uploadId
    update (uploadAdminKey upload) [AdminKeyCount -=. 1]

    -- Removes the corresponding file if it was the last upload.
    file <- updateGet fileId [FileCount -=. 1]
    when (fileCount file < 1) $ do
        -- Removes attributes
        case fileType file of
            Image   -> do
                deleteWhere [ImageAttrsFile ==. fileId]
                deleteWhere [ExifTagFile ==. fileId]
            Audio   -> do
                deleteWhere [MediaAttrsFile ==. fileId]
                deleteWhere [AudioAttrsFile ==. fileId]
            Video   ->
                deleteWhere [MediaAttrsFile ==. fileId]
            Archive ->
                deleteWhere [ArchiveFileFile ==. fileId]
            _       -> return ()

        -- Doesn't remove the jobs as they are kept as a log.

        delete fileId

        app <- lift $ getYesod
        let dir = uploadDir app (fileHash file)
        liftIO $ removeDirectoryRecursive dir
