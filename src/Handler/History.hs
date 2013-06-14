{-# LANGUAGE OverloadedStrings #-}
module Handler.History (getHistoryR, getHistoryFusionR)
    where

import Import

import Control.Monad
import Data.Time.Clock (getCurrentTime, diffUTCTime)

import Account
import Util.Extras (getFileExtras, getIcon)
import Util.Hmac (Hmac (..))
import Util.Pretty (
      PrettyDiffTime (..), PrettyFileSize (..), PrettyNumber (..), wrappedText
    )

-- | Shows the user's upload history.
getHistoryR :: Handler RepHtml
getHistoryR = do
    mUser <- getUser
    rdr <- getUrlRenderParams

    -- Retrieve every upload linked with the user or only those which are linked
    -- to the client's cookie if it's an anonymous user.
    mAdminKey <- getAdminKey
    (uploads, orphans) <- case mAdminKey of
        Just adminKey -> runDB $ do
            let (selectFilters, orphans) = case adminKey of
                    AdminKeyUser (Entity a _) (Just (Entity b anonKey)) ->
                        ([UploadAdminKey ==. a] ||. [UploadAdminKey ==. b]
                        , adminKeyCount anonKey)
                    AdminKeyUser (Entity a _) Nothing             ->
                        ([UploadAdminKey ==. a], 0)
                    AdminKeyAnon (Entity a _)                     ->
                        ([UploadAdminKey ==. a], 0)
            uploads <- selectList selectFilters [Desc UploadId]

            uploads' <- forM uploads $ \(Entity _ upload) -> do
                let fileId = uploadFile upload
                Just file <- get fileId
                extras <- getFileExtras (Entity fileId file)
                return (upload, file, getIcon rdr upload extras)

            return (uploads', orphans)
        Nothing -> return ([], 0)

    app <- getYesod
    currentTime <- liftIO $ getCurrentTime
    defaultLayout $ do
        setTitle "Upload history | getwebb"
        $(widgetFile "public-private")
        $(widgetFile "remove")
        $(widgetFile "history")

-- | Associates each anonymous upload to the signed in user.
getHistoryFusionR :: Handler ()
getHistoryFusionR = do
    mAdminKey <- getAdminKey
    case mAdminKey of
        Just (AdminKeyUser (Entity userKey _) (Just (Entity anonKey c))) ->
            runDB $ do
                updateWhere [UploadAdminKey ==. anonKey]
                            [UploadAdminKey =.  userKey]
                update anonKey [AdminKeyCount =.  0]
                update userKey [AdminKeyCount +=. adminKeyCount c]
        _ -> return ()

    redirect HistoryR
