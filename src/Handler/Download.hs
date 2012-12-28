{-# LANGUAGE OverloadedStrings #-}
module Handler.Download (getDownloadR)
    where

import Import

import qualified Control.Exception as E
import Control.Monad
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T
import Data.Maybe

import Blaze.ByteString.Builder.ByteString (fromLazyByteString)
import Codec.Compression.GZip (decompress)
import Network.Wai (Response (..), requestHeaders)

import Handler.Utils (splitCommas)
import Upload.Path (ObjectType (..), hashDir, uploadDir, getPath)

-- Streams the content of a file over HTTP.
getDownloadR :: Text -> Handler ()
getDownloadR hmac = do
    mRequestType <- parseType <$> lookupGetParam "type"
    when (isNothing mRequestType)
        notFound
    let Just requestType = mRequestType

    app <- getYesod
    (compressed, bs) <- runDB $ do
        mFile <- getBy $ UniqueHmac hmac

        when (isNothing mFile) $ 
            lift notFound

        let Just file = mFile
            dir = hashDir (uploadDir app) (T.unpack $ fileSha1 file)
            compressed = requestType == Original && fileCompressed file

        -- Opens the file inside the transaction to ensure data consistency.
        bs <- lift $ openFile (getPath dir requestType)

        return (compressed, bs)

    if compressed
        then do
            allowGzip <- getGzipClientSupport
            if allowGzip
                then do
                    setHeader "Content-Encoding" "gzip"
                    sendByteString bs
                else
                    sendByteString $ decompress bs
        else
            sendByteString bs
  where
    parseType Nothing            = Just Original
    parseType (Just "")          = Just Original
    parseType (Just "miniature") = Just Miniature
    parseType (Just "webm")      = Just WebM
    parseType (Just "mkv")       = Just MKV
    parseType (Just "mp3")       = Just MP3
    parseType (Just "png")       = Just PNG
    parseType _                  = Nothing

    -- Try to open the file. 404 Not found if doesn't exists.
    openFile path = do
        eBs <- liftIO $ E.try (B.readFile path)
        case eBs of
            Right bs -> return bs
            _        -> notFound

    getGzipClientSupport = do
        request <- waiRequest
        let headers = requestHeaders request
        case "accept-encoding" `lookup` headers of
            Just str ->
                let encodings = splitCommas $ C.unpack encodings
                in return $ "gzip" `elem` encodings
            Nothing -> return False

    sendByteString bs = sendResponse (typeOctet, fromLazyByteString bs)