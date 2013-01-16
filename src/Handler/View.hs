{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
module Handler.View (getViewR)
    where

import Import

import Control.Monad
import Data.Char (toLower)
import Data.List (head, tail, last, init)
import Data.Maybe
import qualified Data.ByteString as B
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import System.FilePath (takeExtension)

import Network.HTTP.Types.Header (hUserAgent)
import Network.Wai (requestHeaders)
import Text.Hamlet (shamlet)

import Handler.Download (routeType, getBufferEntry)
import Handler.Utils (
      PrettyNumber (..), PrettyFileSize (..), PrettyDuration (..)
    , PrettyDiffTime (..)
    , wrappedText, splitHmacs, joinHmacs
    )

import Upload.Archive (archiveTree, treeToHtml)

-- | Used to retrieve the attributes about each file type from the database.
data Extras = ImageExtras ImageAttrs [ExifTag]
            | AudioExtras MediaAttrs (Maybe AudioAttrs)
            | VideoExtras MediaAttrs
            | ArchiveExtras [ArchiveFile]
            | None

-- | Shows information about a file.
getViewR :: Text -> Handler RepHtml
getViewR hmacsJoined = do
    when (null hmacs)
        notFound

    let hmac = head hmacs

    -- Redirects immediately to the download handler if the client is wget.
    mUserAgent <- ((hUserAgent `lookup`). requestHeaders) <$> waiRequest
    case mUserAgent of
        Just userAgent | "Wget/" `B.isPrefixOf` userAgent ->
            redirect (DownloadR hmac)
        _ -> 
            return ()

    (uploadId, upload, file, extras) <- runDB $ do
        mUpload <- getBy $ UniqueUploadHmac hmac

        case mUpload of
            Just (Entity uploadId upload) -> do
                let fileId = uploadFileId upload
                Just file <- get fileId

                extras <- case fileType file of
                    Image -> do
                        Just attrs <- getBy $ UniqueImageAttrs fileId
                        tags <- selectList [ExifTagFileId ==. fileId] []
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
                        files <- selectList [ArchiveFileFileId ==. fileId] []
                        return $ ArchiveExtras (map entityVal files)
                    _ -> return None

                return (uploadId, upload, file, extras)
            Nothing -> do
                -- The file has been removed, redirects to the first existing
                -- file.
                existing <- dropRemoved (tail hmacs)
                if null existing
                    then lift notFound
                    else lift $ redirect $ ViewR $ joinHmacs existing

    rdr <- getUrlRenderParams
    currentUrl <- (flip rdr [] . fromJust) <$> getCurrentRoute
    currentTime <- liftIO $ getCurrentTime
    stats <- getUploadStats uploadId upload
    facebookAppId <- extraFacebook <$> getExtra
    let name = uploadName upload
        wrappedName = wrappedText name 35
        icon = getIcon rdr upload extras
        image = getImage rdr upload extras
        miniature = getMiniature rdr upload extras
        audioSources = getAudioSources rdr upload extras
        archive = getArchive rdr upload extras
        uploadDiffTime = currentTime `diffUTCTime` (uploadUploaded upload)

    defaultLayout $ do
        setTitle [shamlet|#{wrappedName} - getwebb|]
        let right_pane = $(widgetFile "view-right-pane")
            page = case fileType file of
                Image -> $(widgetFile "view-image")
                _     -> $(widgetFile "view-file")

        $(widgetFile "view")
  where
    hmacs = splitHmacs hmacsJoined

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

    -- Returns the statistics of the upload with the uncommited data.
    getUploadStats uploadId upload = do
        let views = uploadViews upload
            lastView = uploadLastView upload
            bw = uploadBandwidth upload

        mBuffer <- getBufferEntry uploadId
        return $! case mBuffer of
            Just (bufViews, mBuffLastView, bufBw) ->
                (views + bufViews, fromMaybe lastView mBuffLastView, bw + bufBw)
            Nothing -> (views, lastView, bw)

    -- Returns the URL to the file icon corresponding to the type of the file.
    getIcon rdr upload extras 
        | Just miniature <- getMiniature rdr upload extras = miniature
    getIcon rdr _      (AudioExtras _ _) = renderStatic rdr img_types_audio_png
    getIcon rdr _      (VideoExtras _)   = renderStatic rdr img_types_video_png
    getIcon rdr _      (ArchiveExtras _) =
        renderStatic rdr img_types_archive_png
    getIcon rdr upload  _                = -- Selects from extension.
        let ext = map toLower $ takeExtension $ T.unpack $ uploadName upload
        in renderStatic rdr $ case () of
            _ | ext == ".pdf"                   -> img_types_pdf_png
              | ext `S.member` extsArchives     -> img_types_archive_png
              | ext `S.member` extsAudio        -> img_types_audio_png
              | ext `S.member` extsCode         -> img_types_code_png
              | ext `S.member` extsExecutable   -> img_types_executable_png
              | ext `S.member` extsImage        -> img_types_image_png
              | ext `S.member` extsPresentation -> img_types_presentation_png
              | ext `S.member` extsSpreadsheet  -> img_types_spreadsheet_png
              | ext `S.member` extsText         -> img_types_text_png
              | ext `S.member` extsVector       -> img_types_vector_png
              | ext `S.member` extsVideo        -> img_types_video_png
              | otherwise                       -> img_types_unknown_png

    renderStatic rdr ressource = rdr (StaticR ressource) []

    -- Returns the URL to the displayable image if the file had one.
    getImage rdr upload (ImageExtras attrs _) =
        case imageAttrsDisplayable attrs of
            Just t  -> Just $ routeType rdr upload (Display t)
            Nothing -> Just $ routeType rdr upload Original
    getImage _   _      _                     = Nothing

    -- Returns the URL to the miniature if the file had one.
    getMiniature rdr upload (ImageExtras _ _) =
        Just $ routeType rdr upload Miniature
    getMiniature rdr upload (AudioExtras _ (Just attrs))
        | audioAttrsMiniature attrs           =
            Just $ routeType rdr upload Miniature
    getMiniature _   _      _                 = Nothing

    -- Returns the URL to every HTML5 audio files which can be displayed in the
    -- browser with thier mime-types.
    getAudioSources rdr upload (AudioExtras _ _) = [
          (routeType rdr upload WebMAudio, "audio/webm" :: Text)
        , (routeType rdr upload MP3      , "audio/mpeg")
        ]
    getAudioSources _   _      _                 = []

    -- Returns the content of the archive as a hierarchical structure.
    getArchive rdr upload (ArchiveExtras files) =
        let rdr' fileHmac = routeType rdr upload (CompressedFile fileHmac)
        in Just $ treeToHtml rdr' (archiveTree files)
    getArchive _   _      _                     = Nothing

    extsArchives = S.fromDistinctAscList [".7z", ".bz", ".deb", ".gz", ".pkg", ".rar"
        , ".rpm", ".tar", ".xz", ".zip"]
    extsAudio = S.fromDistinctAscList [".flac", ".m3u", ".m4a", ".mid", ".mp3", ".ogg"
        , ".wav", ".wma"]
    extsCode = S.fromDistinctAscList [".asp", ".aspx", ".c", ".cfm", ".cpp", ".css"
        , ".cxx", ".dtd", ".fla", ".h", ".hs", ".htm", ".html", ".java", ".js"
        , ".jsp", ".lua", ".m", ".php", ".pl", ".py", ".xhtml", ".xml", ".xs"]
    extsExecutable = S.fromDistinctAscList [".a", ".apk", ".app", ".bat", ".bin", ".cab"
        , ".cgi", ".class", ".com", ".cue", ".dll", ".exe", ".iso", ".jar"
        , ".msi", ".nes", ".o", ".pif", ".rom", ".run", ".sh", ".so", ".wsf"]
    extsImage = S.fromDistinctAscList [".bmp", ".gif", ".ico", ".jpeg", ".jpg", ".png"
        , ".psd", ".tga", ".tif", ".tiff", ".xcf"]
    extsPresentation = S.fromDistinctAscList [".pps", ".ppt", ".pptx"]
    extsSpreadsheet = S.fromDistinctAscList [".accdb", ".db", ".dbf", ".mdb", ".mdb"
        , ".ods", ".pdb", ".sql", ".xlr", ".xls", ".xlsx"]
    extsText = S.fromDistinctAscList [".csv", ".doc", ".docx", ".log", ".msg", ".odt"
        , ".rtf", ".tex", ".wps"]
    extsVector = S.fromDistinctAscList [".ai", ".eps", ".ps", ".svg", ".ttf"]
    extsVideo = S.fromDistinctAscList [".3g2", ".3gp", ".asf", ".asx", ".avi", ".flv"
        , ".mov", ".mp4", ".mpg", ".ogv", ".swf", ".vob", ".webm", ".wmv"]

    -- This is used in hamlet templates because they only supports functions
    -- in expressions, not operators.
    if' :: Bool -> Int -> Int -> Int
    if' True  a _ = a
    if' False _ b = b


int :: Integral a => a -> Int
int = fromIntegral