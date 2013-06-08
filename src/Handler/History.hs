{-# LANGUAGE OverloadedStrings #-}
module Handler.History (getHistoryR)
    where

import Import

import Control.Monad
import Data.Time.Clock (getCurrentTime, diffUTCTime)

import Account
import Util.Extras (getFileExtras, getIcon)
import Util.Hmac (Hmac (..))
import Util.Pretty (PrettyFileSize (..), PrettyDiffTime (..), wrappedText)

-- | Shows the user's upload history.
getHistoryR :: Handler RepHtml
getHistoryR = do
    mAdminKey <- getAdminKey
    mUser <- getUser
    rdr <- getUrlRenderParams

    -- Retrieve every upload linked with the user or only those which are linked
    -- to the client's cookie if it's an anonymous user.
    uploads <- case mAdminKey of
        Just adminKey -> runDB $ do
            let selectFilters = case adminKey of
                    AdminKeyUser (Entity a _) (Just (Entity b _)) ->
                        [UploadAdminKey ==. a] ||. [UploadAdminKey ==. b]
                    AdminKeyUser (Entity a _) Nothing             ->
                        [UploadAdminKey ==. a]
                    AdminKeyAnon (Entity a _)                     ->
                        [UploadAdminKey ==. a]
            uploads <- selectList selectFilters [Desc UploadId]

            forM uploads $ \(Entity _ upload) -> do
                let fileId = uploadFile upload
                Just file <- get fileId
                extras <- getFileExtras (Entity fileId file)
                return (upload, file, getIcon rdr upload extras)
        Nothing -> return []

    app <- getYesod
    currentTime <- liftIO $ getCurrentTime
    defaultLayout $ do
        setTitle "Upload history | getwebb"
        $(widgetFile "remove")
        $(widgetFile "history")
