{-# LANGUAGE OverloadedStrings #-}
module Handler.History (getHistoryR)
    where

import Import

import Control.Monad
import Data.Time.Clock (getCurrentTime, diffUTCTime)

import Account
import Util.Extras (getFileExtras, getIcon)
import Util.Pretty (PrettyFileSize (..), PrettyDiffTime (..), wrappedText)

-- | Shows the user's upload history.
getHistoryR :: Handler RepHtml
getHistoryR = do
    mAdminKey <- tryAdminKey
    mUser <- getUser
    rdr <- getUrlRenderParams

    uploads <- case mAdminKey of
        Just adminKey ->
            runDB $ do
                uploads <- selectList [UploadAdminKey ==. adminKey]
                                      [Desc UploadId]

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
