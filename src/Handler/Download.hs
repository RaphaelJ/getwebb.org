module Handler.Download (getDownloadR)
    where

import Import

import Data.Maybe

import Upload.Utils (hashDir, uploadDir)

-- Streams the content of a file over HTTP.
getDownloadR :: Text -> Handler ()
getDownloadR hmac = do
    mFiletype <- parseType <$> lookupGetParam "type"
    when (isNothing mFiletype)
        notFound
    let filetype = mFiletype

    app <- getYesod
    (file, handle) <- runDB $ do
        mFile <- getBy $ UniqueHmac hmac

        when (isNothing mFile)
            notFound

        -- Opens the file inside the transaction to ensure data consistency.
        let Just file = mFile
            dir = hashDir (uploadDir app) (T.unpack $ fileSha1 file)

        case of 

  where
    parseType Nothing            = Just Original
    parseType (Just "")          = Just Original
    parseType (Just "miniature") = Just Miniature
    parseType (Just "webm")      = Just WebM
    parseType (Just "mkv")       = Just MKV
    parseType (Just "mp3")       = Just MP3
    parseType (Just "png")       = Just PNG
    parseType _                  = Nothing
