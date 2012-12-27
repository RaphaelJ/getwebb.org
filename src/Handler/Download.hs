module Handler.Download (getDownloadR)
    where

import Import

import Data.Maybe

import Upload.Utils (hashDir, uploadDir)

-- | Used to represents the different items which can be downloaded.
data ObjectType = Original | Miniature | WebM | MKV | MP3 | PNG

-- Streams the content of a file over HTTP.
getDownloadR :: Text -> Handler ()
getDownloadR hmac = do
    app <- getYesod

    (file, handle) <- runDB $ do
        mFile <- getBy $ UniqueHmac hmac

        when (isNothing mFile)
            notFound

        -- Opens the file inside the transaction to
        let Just file = mFile
            dir = hashDir (uploadDir app) (T.unpack $ fileSha1 file)

