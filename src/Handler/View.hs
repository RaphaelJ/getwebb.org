{-# LANGUAGE OverloadedStrings #-}
module Handler.View (getViewR)
    where

import Import

import Control.Monad
import Data.List (head, tail, last, init)
import qualified Data.Set as S
import qualified Data.Text as T
import System.FilePath (takeExtension)

import Text.Hamlet (shamlet)

import Handler.Download (ObjectType (..), routeType)
import Handler.Utils (
      PrettyNumber (..), PrettyFileSize (..), wrappedText, splitHmacs, joinHmacs
    )

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

    (upload, file, extras) <- runDB $ do
        mUpload <- getBy $ UniqueHmac hmac

        case mUpload of
            Just (Entity _ upload) -> do
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

                return (upload, file, extras)
            Nothing -> do
                -- The file has been removed, redirects to the first existing
                -- file.
                existing <- dropRemoved (tail hmacs)
                if null existing
                    then lift notFound
                    else lift $ redirect $ ViewR $ joinHmacs existing

    let name = uploadName upload
        wrappedName = wrappedText name 35
    iconUrl <- getFileIcon upload extras

    defaultLayout $ do
        setTitle [shamlet|#{wrappedName} | getwebb | Free file sharing|]
        $(widgetFile "view")
  where
    hmacs = splitHmacs hmacsJoined

    -- Removes the non existing hmacs from the head of the list.
    dropRemoved []         = return []
    dropRemoved hs@(h:hs') = do
        mUpload <- getBy $ UniqueHmac h
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

    -- Returns the route to the file icon corresponding to the type of the file.
    getFileIcon upload (ImageExtras _ _) = routeType upload Miniature
    getFileIcon upload (AudioExtras _ (Just attrs))
        | audioAttrsMiniature attrs      = routeType upload Miniature
    getFileIcon _      (AudioExtras _ _) = renderStatic img_types_audio_png
    getFileIcon _      (VideoExtras _)   = renderStatic img_types_video_png
    getFileIcon _      (ArchiveExtras _) = renderStatic img_types_archive_png
    getFileIcon upload  _                = -- Selects from extension.
        renderStatic $ case takeExtension $ T.unpack $ uploadName upload of
            ext | ext == ".pdf"                   -> img_types_pdf_png
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

    renderStatic ressource = getUrlRender <*> pure (StaticR ressource)

    extsArchives = S.fromAscList [".7z", ".bz", ".deb", ".gz", ".pkg", ".rar"
        , ".rpm", ".tar", ".xz", ".zip"]
    extsAudio = S.fromAscList [".flac", ".m3u", ".m4a", ".mid", ".mp3", ".ogg"
        , ".wav", ".wma"]
    extsCode = S.fromAscList [".asp", ".aspx", ".c", ".cfm", ".cpp", ".css"
        , ".cxx", ".dtd", ".fla", ".h", ".hs", ".htm", ".html", ".java", ".js"
        , ".jsp", ".lua", ".m", ".php", ".pl", ".py", ".xhtml", ".xml", ".xs"]
    extsExecutable = S.fromAscList [".a", ".apk", ".app", ".bat", ".bin", ".cab"
        , ".cgi", ".class", ".com", ".cue", ".dll", ".exe", ".iso", ".jar"
        , ".nes", ".o", ".pif", ".rom", ".run", ".sh", ".so", ".wsf"]
    extsImage = S.fromAscList [".bmp", ".gif", ".ico", ".jpeg", ".jpg", ".png"
        , ".psd", ".tga", ".tif", ".tiff", ".xcf"]
    extsPresentation = S.fromAscList [".pps", ".ppt", ".pptx"]
    extsSpreadsheet = S.fromAscList [".accdb", ".db", ".dbf", ".mdb", ".mdb"
        , ".pdb", ".sql", ".xlr", ".xls", ".xlsx"]
    extsText = S.fromAscList [".csv", ".doc", ".docx", ".log", ".msg", ".odt"
        , ".rtf", ".tex", ".wps"]
    extsVector = S.fromAscList [".ai", ".eps", ".ps", ".svg", ".ttf"]
    extsVideo = S.fromAscList [".3g2", ".3gp", ".asf", ".asx", ".avi", ".flv"
        , ".mov", ".mp4", ".mpg", ".ogv", ".swf", ".vob", ".webm", ".wmv"]

int :: Integral a => a -> Int
int = fromIntegral