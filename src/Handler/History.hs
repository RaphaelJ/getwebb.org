{-# LANGUAGE OverloadedStrings #-}
module Handler.History (getHistoryR, getHistoryFusionR)
    where

import Import

import Account
import Util.Extras (getUploadsInfo)
import Util.Hmac (Hmac (..))
import Util.Paging (getPageOffset, pagingWidget)
import Util.Pretty (PrettyFileSize (..), PrettyNumber (..), wrappedText)

uploadsPerPage :: Int
uploadsPerPage = 24

-- | Shows the user's upload history.
getHistoryR :: Handler RepHtml
getHistoryR = do
    app <- getYesod
    mUser <- getUser
    rdr <- getUrlRenderParams

    -- Retrieve every upload linked with the user or only those which are linked
    -- to the client's cookie if it's an anonymous user.
    mAdminKey <- getAdminKey
    (page, selectOpts) <- getPageOffset uploadsPerPage
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

            uploads <- selectList selectFilters (Desc UploadId : selectOpts) >>=
                       getUploadsInfo (Just adminKey)

            return (uploads, orphans)
        Nothing -> return ([], 0)

    defaultLayout $ do
        let currentUserPage = UserHistory
            userBarWidget   = $(widgetFile "modules/user-bar")
            uploadsWidget   = $(widgetFile "modules/uploads-list")

        setTitle "Upload history | getwebb"
        $(widgetFile "modules/upload-public-private")
        $(widgetFile "modules/upload-remove")
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
