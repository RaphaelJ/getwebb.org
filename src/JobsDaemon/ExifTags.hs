{-# LANGUAGE BangPatterns #-}
-- | Uses the JobDaemon to fetch the EXIF tags of an image in background.
module JobsDaemon.ExifTags (
      exifTags
    -- * Job
    , jobExifTags
    -- * Background compression queue management
    , putFile
    ) where

import Import

import JobsDaemon.Util (registerJob, runDBIO)

-- | Reads EXIF tags from the image. Returns an empty list if the image doesn't
-- support EXIF tags.
exifTags :: FilePath -> IO [(Text, Text)]
exifTags path = do
    eTags <- E.try $ do
        tags <- fromFile path >>= allTags
        forM tags $ \(!name, !value) -> do
            title <- tagFromName name >>= tagTitle
            E.evaluate (T.pack title, T.pack value)

    case eTags of
        Right tags -> return tags
        Left (_ :: E.SomeException) -> return []

-- | Reads and saves the EXIF tags of an image in the database.
jobExifTags :: App -> FileId -> IO ()
jobExifTags app fileId = do
    Just file <- runDBIO app $ get fileId

    let path = getPath (uploadDir app (fileHash file)) Original

    tags <- exifTags path

    when (not $ null tags) $ runDBIO app $ do
        mFile <- get fileId -- Fails if the file doesn't exist.
        case mFile of
            Just _  -> error "File doesn't exists anymore"
            Nothing ->
                forM_ tags $ \(title, value) ->
                    insertUnique $ ExifTag fileId title value

-- | Adds a file to the background EXIF tags extracting queue.
putFile :: App -> FileId -> [JobId] -> IO JobId
putFile app fileId deps = registerJob app fileId deps (jobExifTags app fileId)
