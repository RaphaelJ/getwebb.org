{-# LANGUAGE OverloadedStrings, PatternGuards #-}
-- | Page which displays the information about a file.
module Handler.View (getViewR, patchViewR, deleteViewR, removeUpload)
    where

import Import

import Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.List (head, tail, last, init)
import Data.Maybe
import qualified Data.Text as T

import Network.HTTP.Types.Header (hUserAgent)
import Network.Wai (requestHeaders)
import System.Directory (removeDirectoryRecursive)
import Text.Hamlet (shamlet)
import Text.Julius (rawJS)

import Account
import Handler.Comment (
      maxCommentLength, commentForm, retrieveComments, removeComment
    )
import Util.API (sendNoContent, withFormSuccess, withUploadOwner)
import Util.Date (getDiffTime)
import Util.Pretty (
      PrettyNumber (..), PrettyFileSize (..), PrettyDuration (..)
    , PrettyDiffTime (..)
    , wrappedText
    )
import Util.Extras (
      Extras (..), getFileExtras, getUploadStats, getIcon, getImage
    , getMiniature, getAudioSources, getArchive, getUploadOwner
    )
import Util.Hmac (Hmac (..), Hmacs (..))
import Util.Path (uploadDir)

-- Handlers --------------------------------------------------------------------

-- | Shows information about an upload.
getViewR :: Text -> Handler RepHtml
getViewR hmacs' = do
    case fromPathPiece hmacs' of
        Just (Hmacs hmacs@(hmac:_)) -> do
            redirectWget

            mUser <- getUser
            let mUserId = (entityKey . fst) <$> mUser

            -- Retrieves the first file and its comments.
            (entity, file, extras, mOwner, comments) <- runDB $ do
                mUpload <- getBy $ UniqueUploadHmac hmac

                case mUpload of
                    Just entity@(Entity uploadId upload) -> do
                        let fileId = uploadFile upload
                        Just file <- get fileId

                        mOwner <- getUploadOwner upload
                        extras <- getFileExtras (Entity fileId file)

                        -- Retrieves comments.
                        comments' <- retrieveComments mUserId uploadId Nothing
                                                     Nothing
                        comments <- forM comments' $ \(c, author, v) -> do
                            let Entity _ comment = c
                            diffTime <- getDiffTime $ commentCreated comment
                            let isOwner = Just (entityKey author) == mUserId
                            Just avatar <- getAvatar $ entityVal author
                            return (c, diffTime, author, isOwner, avatar, v)

                        return (entity, file, extras, mOwner, comments)
                    Nothing -> redirectNext (tail hmacs)
            let Entity _ upload = entity

            app <- getYesod
            mAdminKey <- getAdminKey
            rdr <- getUrlRenderParams
            Just currUrl <- getCurrentRoute
            stats <- getUploadStats entity
            facebookAppId <- extraFacebook <$> getExtra
            (commentWidget, commentEnctype) <- generateFormPost commentForm

            let name = uploadName upload
                wrappedName = wrappedText name 50
                links = getLinks hmacs
                icon = getIcon upload extras
                image = getImage hmac extras
                miniature = getMiniature hmac extras
                audioSources = getAudioSources hmac extras
                archive = getArchive rdr hmac extras
                headersHeight = if isJust links then 67 else 37 :: Int
            uploadDiffTime <- getDiffTime $ uploadCreated upload

            defaultLayout $ do
                setTitle [shamlet|#{wrappedName} | getwebb|]
                let right_pane = $(widgetFile "view-right-pane")
                    page = case fileType file of
                        Image -> $(widgetFile "view-image")
                        _     -> $(widgetFile "view-file")

                $(widgetFile "remove")
                $(widgetFile "view")
        _  -> notFound
  where
    -- Redirects immediately wget clients to the download handler.
    redirectWget = do
        mUserAgent <- ((hUserAgent `lookup`) . requestHeaders) <$> waiRequest
        case mUserAgent of
            Just userAgent | "Wget/" `B.isPrefixOf` userAgent ->
                redirect (DownloadR hmacs')
            _ ->
                return ()

    -- The file has been removed, redirects to the first existing file if any.
    redirectNext hmacs = do
        existing <- dropRemoved hmacs
        if null existing then notFound
                         else redirect $ ViewR $ toPathPiece $ Hmacs existing

    -- Removes the non existing hmacs from the head of the list.
    dropRemoved []         = return []
    dropRemoved hs@(h:hs') = do
        mUpload <- getBy $ UniqueUploadHmac h
        case mUpload of
            Just _  -> return hs
            Nothing -> dropRemoved hs'

    -- If there is more than one file, links contains the list of hmacs for
    -- the previous and for the next file.
    getLinks hmacs@(_:_:_) =
        let previous = toPathPiece $ Hmacs $ last hmacs : init hmacs
            next     = toPathPiece $ Hmacs $ tail hmacs ++ [head hmacs]
        in Just (previous, next)
    getLinks _             = Nothing

    -- This is used in hamlet templates because they only supports functions
    -- in expressions, not operators.
    if' :: Bool -> a -> a -> a
    if' True  a _ = a
    if' False _ b = b

-- | Updates some attributes about an upload. Returns a 204 No content on
-- success, a 400 Bad Request if the POST data are invalid, a 404 Not found if
-- doesn't exists or 403 if the user isn't allowed to remove the upload.
patchViewR :: Text -> Handler ()
patchViewR hmacTxt = do
    ((res, _), _) <- runFormPostNoToken form

    withFormSuccess res $ \(mDescription, mPublic) ->
        withUploadOwner (Hmac hmacTxt) sendNoContent $ \(Entity uploadId _) ->
            update uploadId $ catMaybes [
                  (UploadDescription =.) <$> Just <$> mDescription
                , (UploadPublic =.) <$> mPublic
                ]
  where
    form = renderDivs $ (,) <$> aopt textField descriptionSettings Nothing
                            <*> aopt boolField publicSettings      Nothing

    descriptionSettings = FieldSettings {
          fsLabel = "Upload description", fsTooltip = Nothing, fsId = Nothing
        , fsName = Just "description", fsAttrs = []
        }

    publicSettings = FieldSettings {
          fsLabel = "Share this file", fsTooltip = Nothing, fsId = Nothing
        , fsName = Just "public", fsAttrs = []
        }

-- | Deletes an upload. Returns a 204 No content on success, a 404 Not found if
-- doesn't exists or 403 if the user isn't allowed to remove the upload.
deleteViewR :: Text -> Handler ()
deleteViewR hmacTxt = do
    (Entity userId _, _) <- requireAuth
    withUploadOwner (Hmac hmacTxt) sendNoContent (removeUpload userId)

-- Utilities -------------------------------------------------------------------

-- | Removes an upload. Decrements its owner\'s count and remove the associated
-- file if the file is now upload-less.
removeUpload :: UserId -> Entity Upload -> YesodDB App ()
removeUpload userId (Entity uploadId upload) = do
    -- Removes comments.
    comments <- selectList [CommentUpload ==. uploadId] []
    forM_ comments (removeComment userId)

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

        app <- getYesod
        let dir = uploadDir app (fileHash file)
        liftIO $ removeDirectoryRecursive dir
