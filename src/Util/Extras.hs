{-# LANGUAGE OverloadedStrings, PatternGuards #-}
-- | Helpers to retrieve meta-data and statistics from a file/upload.
module Util.Extras (
      Extras (..), getFileExtras, getUploadStats
    , getIcon, getImage, getMiniature, getAudioSources, getArchive
    , getUploadInfo, getUploadsInfo, getUploadOwner
    ) where

import Import

import Control.Monad.Trans.Maybe
import Data.Char (toLower)
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import System.FilePath (takeExtension)

import Account (Avatar, getAvatar)
import Handler.Download.ViewsCache (ViewsCacheEntry (..), getCacheEntry)
import Handler.Upload.Archive (archiveTree, treeToHtml)
import Util.Hmac (Hmac (..))

-- | Used to retrieve the attributes about each file type from the database.
data Extras = ImageExtras ImageAttrs [ExifTag]
            | AudioExtras MediaAttrs (Maybe AudioAttrs)
            | VideoExtras MediaAttrs
            | ArchiveExtras [ArchiveFile]
            | None

-- | Returns the meta data about a file.
getFileExtras :: Entity File -> YesodDB App Extras
getFileExtras (Entity fileId file) =
    case fileType file of
        Image -> do
            Just attrs <- getBy $ UniqueImageAttrs fileId
            tags <- selectList [ExifTagFile ==. fileId] []
            return $ ImageExtras (entityVal attrs)
                                 (map entityVal tags)
        Audio -> do
            Just attrs <- getBy $ UniqueMediaAttrs fileId
            audioAttrs <- getBy $ UniqueAudioAttrs fileId
            return $ AudioExtras (entityVal attrs)
                                 (entityVal <$> audioAttrs)
        Video -> do
            Just attrs <- getBy $ UniqueMediaAttrs fileId
            return $ VideoExtras (entityVal attrs)
        Archive -> do
            files <- selectList [ArchiveFileFile ==. fileId] []
            return $ ArchiveExtras (map entityVal files)
        _ -> return None


-- | Returns the statistics of the upload, taking in account the uncommited
-- database data (@(views, last view date, bandwidth)@).
getUploadStats :: Entity Upload -> Handler (Word64, UTCTime, Word64)
getUploadStats (Entity uploadId upload) = do
    let views = uploadViews upload
        lastView = uploadViewed upload
        bw = uploadBandwidth upload

    mEntry <- getCacheEntry uploadId
    return $! case mEntry of
        Just (ViewsCacheEntry (Just (cacheViews, cacheLastView)) cacheBw) ->
            (views + cacheViews, cacheLastView, cacheBw)
        Just (ViewsCacheEntry Nothing                            cacheBw) ->
            (views, lastView, bw + cacheBw)
        Nothing -> (views, lastView, bw)

-- | Returns the URL to the file icon corresponding to the type of the file.
getIcon :: Upload -> Extras -> Route App
getIcon upload extras =
    case extras of
        _   | Just miniature <- getMiniature hmac extras -> miniature
        AudioExtras _ _ -> StaticR img_types_audio_png
        VideoExtras _   -> StaticR img_types_video_png
        ArchiveExtras _ -> StaticR img_types_archive_png
        _                -- Selects from extension.
            | ext == ".pdf"                   -> StaticR img_types_pdf_png
            | ext `S.member` extsArchives     -> StaticR img_types_archive_png
            | ext `S.member` extsAudio        -> StaticR img_types_audio_png
            | ext `S.member` extsCode         -> StaticR img_types_code_png
            | ext `S.member` extsExecutable   -> StaticR img_types_executable_png
            | ext `S.member` extsImage        -> StaticR img_types_image_png
            | ext `S.member` extsPresentation -> StaticR img_types_presentation_png
            | ext `S.member` extsSpreadsheet  -> StaticR img_types_spreadsheet_png
            | ext `S.member` extsText         -> StaticR img_types_text_png
            | ext `S.member` extsVector       -> StaticR img_types_vector_png
            | ext `S.member` extsVideo        -> StaticR img_types_video_png
            | otherwise                       -> StaticR img_types_unknown_png
  where
    hmac = uploadHmac upload

    ext = map toLower $ takeExtension $ T.unpack $ uploadName upload

    extsArchives = S.fromDistinctAscList [".7z", ".bz", ".deb", ".gz", ".pkg"
        , ".rar", ".rpm", ".tar", ".xz", ".zip"]
    extsAudio = S.fromDistinctAscList [".flac", ".m3u", ".m4a", ".mid", ".mp3"
        , ".ogg", ".wav", ".wma"]
    extsCode = S.fromDistinctAscList [".asp", ".aspx", ".c", ".cfm", ".cpp"
        , ".css", ".cxx", ".dtd", ".fla", ".h", ".hs", ".htm", ".html", ".java"
        , ".js", ".jsp", ".lua", ".m", ".php", ".pl", ".py", ".xhtml", ".xml"
        , ".xs"]
    extsExecutable = S.fromDistinctAscList [".a", ".apk", ".app", ".bat", ".bin"
        , ".cab", ".cgi", ".class", ".com", ".cue", ".dll", ".exe", ".iso"
        , ".jar", ".msi", ".nes", ".o", ".pif", ".rom", ".run", ".sh", ".so"
        , ".wsf"]
    extsImage = S.fromDistinctAscList [".bmp", ".gif", ".ico", ".jpeg", ".jpg"
        , ".png", ".psd", ".tga", ".tif", ".tiff", ".xcf"]
    extsPresentation = S.fromDistinctAscList [".pps", ".ppt", ".pptx"]
    extsSpreadsheet = S.fromDistinctAscList [".accdb", ".db", ".dbf", ".mdb"
        , ".mdb", ".ods", ".pdb", ".sql", ".xlr", ".xls", ".xlsx"]
    extsText = S.fromDistinctAscList [".csv", ".doc", ".docx", ".log", ".msg"
        , ".odt", ".rtf", ".tex", ".wps"]
    extsVector = S.fromDistinctAscList [".ai", ".eps", ".ps", ".svg", ".ttf"]
    extsVideo = S.fromDistinctAscList [".3g2", ".3gp", ".asf", ".asx", ".avi"
        , ".flv", ".mov", ".mp4", ".mpg", ".ogv", ".swf", ".vob", ".webm"
        , ".wmv"]

-- Returns the URL to the displayable image if the file has one.
getImage :: Hmac -> Extras -> Maybe (Route App)
getImage hmac@(Hmac hmacTxt) (ImageExtras attrs _) =
    case imageAttrsDisplayable attrs of
        Just _  -> Just $ DownloadDisplayableR hmac
        Nothing -> Just $ DownloadR            hmacTxt
getImage _                   _                     = Nothing

-- | Returns the URL to the miniature if the file has one.
getMiniature :: Hmac -> Extras -> Maybe (Route App)
getMiniature hmac (ImageExtras _ _) = Just $ DownloadMiniatureR hmac
getMiniature hmac (AudioExtras _ (Just attrs))
    | audioAttrsMiniature attrs     = Just $ DownloadMiniatureR hmac
getMiniature _    _                 = Nothing

-- | Returns the URL to every HTML5 audio files which can be displayed in the
-- browser with thier mime-types.
getAudioSources :: Hmac -> Extras -> [(Route App, ContentType)]
getAudioSources hmac (AudioExtras _ _) =
    [ (DownloadWebMAR hmac, "audio/webm"), (DownloadMP3R   hmac, "audio/mpeg") ]
getAudioSources _    _                 = []

-- | Returns the content of the archive as a hierarchical HTML structure.
getArchive :: (Route App -> [(Text, Text)] -> Text) -> Hmac -> Extras
           -> Maybe Html
getArchive rdr hmac (ArchiveExtras files) =
    let rdr' archiveHmac = rdr (DownloadArchiveR hmac archiveHmac) []
    in Just $ treeToHtml rdr' $ archiveTree files
getArchive _   _    _                     = Nothing

-- | Fetches the database to retrieve the corresponding file and icon URL for
-- an upload.
getUploadInfo :: Entity Upload -> YesodDB App (Upload, File, Route App)
getUploadInfo (Entity _ upload) = do
    let fileId = uploadFile upload
    Just file <- get fileId
    extras    <- getFileExtras (Entity fileId file)
    return (upload, file, getIcon upload extras)

-- | Fetches the database to retrieve the corresponding file and icon URL for
-- each upload.
getUploadsInfo :: [Entity Upload] -> YesodDB App [(Upload, File, Route App)]
getUploadsInfo = mapM getUploadInfo

-- | Returns the account and of avatar of the user who uploaded the file.
getUploadOwner :: Upload -> YesodDB App (Maybe (Entity User, Avatar))
getUploadOwner upload = runMaybeT $ do
    user   <- MaybeT $ getBy $ UniqueUserAdminKey $ uploadAdminKey upload
    avatar <- MaybeT $ getAvatar $ entityVal user
    return (user, avatar)
