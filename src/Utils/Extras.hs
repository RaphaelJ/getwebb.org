{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
-- | Defines functions to retrieve meta-data from a file.
module Utils.Extras (
      Extras (..), getFileExtras
    , getIsAdmin, getUploadStats, getIcon, getImage, getMiniature
    , getAudioSources, getArchive
    ) where

import Import

import Data.Char (toLower)
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import System.FilePath (takeExtension)

import Handler.Download (routeType, getBufferEntry)
import Upload.Archive (archiveTree, treeToHtml)

-- | Used to retrieve the attributes about each file type from the database.
data Extras = ImageExtras ImageAttrs [ExifTag]
            | AudioExtras MediaAttrs (Maybe AudioAttrs)
            | VideoExtras MediaAttrs
            | ArchiveExtras [ArchiveFile]
            | None

-- | Returns the meta data about a file.
getFileExtras :: Entity File -> YesodDB App App Extras
getFileExtras (Entity fileId file) =
    case fileType file of
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


-- | Returns the statistics of the upload with the uncommited data.
getUploadStats :: Entity Upload -> Handler (Word64, UTCTime, Word64)
getUploadStats (Entity uploadId upload) = do
    let views = uploadViews upload
        lastView = uploadLastView upload
        bw = uploadBandwidth upload

    mBuffer <- getBufferEntry uploadId
    return $! case mBuffer of
        Just (bufViews, mBuffLastView, bufBw) ->
            (views + bufViews, fromMaybe lastView mBuffLastView, bw + bufBw)
        Nothing -> (views, lastView, bw)

-- | Returns True if the client is the administrator of the upload.
getIsAdmin :: Upload -> Handler Bool
getIsAdmin upload = do
    mKey <- tryAdminKey

    return $ case mKey of
        Just key | key == uploadAdminKey upload -> True
        _                                       -> False

-- | Returns the URL to the file icon corresponding to the type of the file.
getIcon :: (Route App -> [(Text, Text)] -> Text) -> Upload -> Extras -> Text
getIcon rdr upload extras =
    case extras of
        _   | Just miniature <- getMiniature rdr upload extras
                        -> miniature
        AudioExtras _ _ -> rdr' img_types_audio_png
        VideoExtras _   -> rdr' img_types_video_png
        ArchiveExtras _ -> rdr' img_types_archive_png
        _                -- Selects from extension.
            | ext == ".pdf"                   -> rdr' img_types_pdf_png
            | ext `S.member` extsArchives     -> rdr' img_types_archive_png
            | ext `S.member` extsAudio        -> rdr' img_types_audio_png
            | ext `S.member` extsCode         -> rdr' img_types_code_png
            | ext `S.member` extsExecutable   -> rdr' img_types_executable_png
            | ext `S.member` extsImage        -> rdr' img_types_image_png
            | ext `S.member` extsPresentation -> rdr' img_types_presentation_png
            | ext `S.member` extsSpreadsheet  -> rdr' img_types_spreadsheet_png
            | ext `S.member` extsText         -> rdr' img_types_text_png
            | ext `S.member` extsVector       -> rdr' img_types_vector_png
            | ext `S.member` extsVideo        -> rdr' img_types_video_png
            | otherwise                       -> rdr' img_types_unknown_png
  where
    rdr' ressource = rdr (StaticR ressource) []
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
getImage :: (Route App -> [(Text, Text)] -> Text) -> Upload -> Extras
         -> Maybe Text
getImage rdr upload (ImageExtras attrs _) =
    case imageAttrsDisplayable attrs of
        Just t  -> Just $ routeType rdr upload (Display t)
        Nothing -> Just $ routeType rdr upload Original
getImage _   _      _                     = Nothing

-- | Returns the URL to the miniature if the file has one.
getMiniature :: (Route App -> [(Text, Text)] -> Text) -> Upload -> Extras
             -> Maybe Text
getMiniature rdr upload (ImageExtras _ _) =
    Just $ routeType rdr upload Miniature
getMiniature rdr upload (AudioExtras _ (Just attrs))
    | audioAttrsMiniature attrs           =
        Just $ routeType rdr upload Miniature
getMiniature _   _      _                 = Nothing

-- | Returns the URL to every HTML5 audio files which can be displayed in the
-- browser with thier mime-types.
getAudioSources :: (Route App -> [(Text, Text)] -> Text) -> Upload -> Extras
                -> [(Text, Text)]
getAudioSources rdr upload (AudioExtras _ _) = [
      (routeType rdr upload WebMAudio, "audio/webm")
    , (routeType rdr upload MP3      , "audio/mpeg")
    ]
getAudioSources _   _      _                 = []

-- | Returns the content of the archive as a hierarchical HTML structure.
getArchive :: (Route App -> [(Text, Text)] -> Text) -> Upload -> Extras 
           -> Maybe Html
getArchive rdr upload (ArchiveExtras files) =
    let rdr' fileHmac = routeType rdr upload (CompressedFile fileHmac)
    in Just $ treeToHtml rdr' (archiveTree files)
getArchive _   _      _                     = Nothing
