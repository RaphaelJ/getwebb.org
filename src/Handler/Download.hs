module Handler.Download (getDownloadR)
    where

import Import

import qualified Control.Exception as E
import Data.Maybe
import System.IO (openFile)

import Blaze.ByteString.Builder.ByteString (fromLazyByteString)
import Network.Wai (Response (..))

import Upload.Path (ObjectType (..), hashDir, uploadDir, getPath)

-- Streams the content of a file over HTTP.
getDownloadR :: Text -> Handler ()
getDownloadR hmac = do
    mRequestType <- parseType <$> lookupGetParam "type"
    when (isNothing mRequestType)
        notFound
    let Just requestType = mRequestType

    app <- getYesod
    (compressed, handle) <- runDB $ do
        mFile <- getBy $ UniqueHmac hmac

        when (isNothing mFile) $ 
            lift notFound

        let Just file = mFile
            dir = hashDir (uploadDir app) (T.unpack $ fileSha1 file)
            compressed = requestType == Original && fileCompressed file

        -- Opens the file inside the transaction to ensure data consistency.
        handler <- lift $ openHandler (getPath dir requestType)

        return (compressed, handler)

    if compressed
        then do
            
        allowGzip = 
        else do
            
    
    setHeader "" "gzip" ContentEncoding
    let content = ContentBuilder (fromLazyByteString bs) (Just size)
    sendResponse typeOctet content

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
    openHandler path = do
        eHandler <- liftIO $ E.try (openFile path)
        case eHandler of
            Right handler -> return handler
            _             -> notFound

            
            gzip set app env = do
    res <- app env
    case res of
        ResponseFile{} | gzipFiles set == GzipIgnore -> return res
        _ -> if "gzip" `elem` enc && not isMSIE6
                then
                    case (res, gzipFiles set) of
                        (ResponseFile s hs file Nothing, GzipCacheFolder cache) ->
                            case lookup "content-type" hs of
                                Just m
                                    | gzipCheckMime set m -> liftIO $ compressFile s hs file cache
                                _ -> return res
                        _ -> return $ compressE set res
                else return res
  where
    enc = fromMaybe [] $ (splitCommas . S8.unpack)
                    `fmap` lookup "Accept-Encoding" (requestHeaders env)
    ua = fromMaybe "" $ lookup "user-agent" $ requestHeaders env
    isMSIE6 = "MSIE 6" `S.isInfixOf` ua