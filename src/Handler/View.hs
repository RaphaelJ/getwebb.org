{-# LANGUAGE OverloadedStrings, PatternGuards #-}
-- | Page which displays the information about a file.
module Handler.View (getViewR, patchViewR, deleteViewR)
    where

import Import

import Control.Monad
import Data.List (head, tail, last, init)
import Data.Maybe
import qualified Data.ByteString as B

import Network.HTTP.Types.Header (hUserAgent)
import Network.HTTP.Types.Status (noContent204)
import Network.Wai (requestHeaders)
import Text.Hamlet (shamlet)
import Text.Julius (rawJS)

import Handler.Comment (maxCommentLength, commentForm)
import Handler.Upload.Remove (removeUpload)
import Util.Date (getDiffTime)
import Util.Pretty (
      PrettyNumber (..), PrettyFileSize (..), PrettyDuration (..)
    , PrettyDiffTime (..)
    , wrappedText
    )
import Util.Extras (
      Extras (..), getFileExtras, getUploadStats
    , getIcon, getImage, getMiniature, getAudioSources, getArchive
    )
import Util.Hmac (splitHmacs, joinHmacs)

-- | Shows information about an upload.
getViewR :: Hmac -> Handler RepHtml
getViewR hmacs' = do
    when (null hmacs)
        notFound

    let hmac = head hmacs

    -- Redirects immediately wget clients to the download handler.
    mUserAgent <- ((hUserAgent `lookup`) . requestHeaders) <$> waiRequest
    case mUserAgent of
        Just userAgent | "Wget/" `B.isPrefixOf` userAgent ->
            redirect (DownloadR hmacs')
        _ ->
            return ()

    (entity@(Entity _ upload), file, extras) <- runDB $ do
        mUpload <- getBy $ UniqueUploadHmac hmac

        case mUpload of
            Just entity@(Entity _ upload) -> do
                let fileId = uploadFile upload
                Just file <- get fileId

                extras <- getFileExtras (Entity fileId file)

                return (entity, file, extras)
            Nothing -> do
                -- The file has been removed, redirects to the first existing
                -- file.
                existing <- dropRemoved (tail hmacs)
                if null existing
                    then lift notFound
                    else lift $ redirect $ ViewR $ joinHmacs existing

    mAdminKey <- getAdminKey
    rdr <- getUrlRenderParams
    currUrl <- (flip rdr [] . fromJust) <$> getCurrentRoute
    stats <- getUploadStats entity
    facebookAppId <- extraFacebook <$> getExtra
    (newCommentWidget, newCommentEnctype) <- generateFormPost commentForm

    let name = uploadName upload
        wrappedName = wrappedText name 50
        icon = getIcon rdr upload extras
        image = getImage rdr upload extras
        miniature = getMiniature rdr upload extras
        audioSources = getAudioSources rdr upload extras
        archive = getArchive rdr upload extras
    uploadDiffTime <- getDiffTime $ uploadCreated upload

    defaultLayout $ do
        setTitle [shamlet|#{wrappedName} | getwebb|]
        let right_pane = $(widgetFile "view-right-pane")
            page = case fileType file of
                Image -> $(widgetFile "view-image")
                _     -> $(widgetFile "view-file")

        $(widgetFile "remove")
        $(widgetFile "view")
  where
    hmacs = splitHmacs hmacs'

    -- Removes the non existing hmacs from the head of the list.
    dropRemoved []         = return []
    dropRemoved hs@(h:hs') = do
        mUpload <- getBy $ UniqueUploadHmac h
        case mUpload of
            Just _  -> return hs
            Nothing -> dropRemoved hs'

    -- If there is more than one file, links contains the list of hmacs for
    -- the previous and for the next file.
    links | null (tail hmacs) = Nothing
          | otherwise         =
        let previous = last hmacs : init hmacs
            next     = tail hmacs ++ [head hmacs]
        in Just (joinHmacs previous, joinHmacs next)

    -- This is used in hamlet templates because they only supports functions
    -- in expressions, not operators.
    if' :: Bool -> Int -> Int -> Int
    if' True  a _ = a
    if' False _ b = b

-- | Updates some attributes about an upload. Returns a 204 No content on
-- success, a 400 Bad Request if the POST data are invalids, a 404 Not found if
-- doesn't exists or 403 if the user isn't allowed to remove the upload.
patchViewR :: Hmac -> Handler ()
patchViewR hmac = do
    ((res, _), _) <- runFormPost form

    case res of
        FormSuccess (mTitle, mPublic) ->
        FormFailure errs ->
        FormMissing ->
  where
    form = (,) <$> aopt textField titleSettings  Nothing
               <*> aopt boolField publicSettings Nothing

    titleSettings = FieldSettings {
          fsLabel = "Upload title", fsTooltip = Nothing, fsId = Nothing
        , fsName = "title", fsAttrs = []
        }

    publicSettings = FieldSettings {
          fsLabel = "Share this file", fsTooltip = Nothing, fsId = Nothing
        , fsName = Just "public", fsAttrs = []
        }

-- | Deletes an upload. Returns a 204 No content on success, a 404 Not found if
-- doesn't exists or 403 if the user isn't allowed to remove the upload.
deleteViewR :: Hmac -> Handler ()
deleteViewR hmac = do
    mAdminKey <- getAdminKey

    case mAdminKey of
        Just adminKey -> do
            validKey <- runDB $ do
                entity@(Entity _ upload) <- getBy404 $ UniqueUploadHmac hmac

                if isAdmin upload (Just adminKey) then do removeUpload entity
                                                          return True
                                                  else return False
            if validKey
                then sendResponseStatus noContent204 ()
                else 
                    permissionDenied
                        "Your admin key doesn't match the upload's admin key."

        Nothing -> permissionDenied "Your admin key cookie is empty."
