{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Handler.Download (
    -- * Views daemon cache management
      ViewsCache, newCache, uploadView, getCacheEntry
    -- * Starting the daemon
    , viewsCommitDelay, viewsDaemon, forkViewsDaemon
    -- * Page handler
    , getDownloadR
    -- * URL utilities
    , ObjectType (..), routeType
    )where

import Import

import Control.Concurrent (
      ThreadId, MVar, forkIO, newMVar, readMVar, modifyMVar_, swapMVar
     , threadDelay
    )
import qualified Control.Exception as E
import Control.Monad
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as B
import Data.Int
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Text as T
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Word
import System.IO (IOMode (..), openBinaryFile, hFileSize)

import Blaze.ByteString.Builder.ByteString (fromLazyByteString)
import Codec.Compression.GZip (decompress)
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Database.Persist.Store (runPool)
import Network.Wai (requestHeaders)

import Upload.Path (ObjectType (..), hashDir, uploadDir, getPath)

import System.TimeIt

-- | Temporary cache for the count of views and the last view which have not
-- been commited yet to the database.
type ViewsCache = MVar (M.Map UploadId (Word64, UTCTime))

-- | Initialises a new view cache to be inserted in the foundation type.
newCache :: IO ViewsCache
newCache = newMVar M.empty

-- | Updates the cache to increment by one the number of views and to update the
-- last view date.
uploadView :: ViewsCache -> UploadId -> IO ()
mvar `uploadView` uploadId = do
    currentTime <- getCurrentTime

    modifyMVar_ mvar $ \accum -> do
        let f _ (oldCount, _) = (oldCount + 1, currentTime)
        return $! M.insertWith f uploadId (1, currentTime) accum

-- | Returns the cache entry for the given upload. 'Nothing' if the database is
-- up to date.
getCacheEntry :: ViewsCache -> UploadId -> IO (Maybe (Word64, UTCTime))
mvar `getCacheEntry` uploadId = do
    accum <- readMVar mvar
    return $ uploadId `M.lookup` accum

-- | Delay in microseconds at which the views cache is commited to the 
-- database.
viewsCommitDelay :: Int
viewsCommitDelay = 5 * 10^(6 :: Int) -- Every five seconds.

-- | Accumulates the view count and the last view for each file to batch update
-- the database every few seconds (saves a lot of disk accesses).
viewsDaemon :: App -> IO ()
viewsDaemon app = do
    let cache = viewsCache app
    forever $ do
        -- Resets the cache.
        oldCache <- cache `swapMVar` M.empty

        -- Inserts the old cache in the database.
        when (not $ M.null oldCache) $ timeIt $ runDBIO $ do
            forM_ (M.assocs oldCache) $ \(uploadId, (views, lastView)) ->
                update uploadId [
                      UploadViews +=. views, UploadLastView =. lastView
                    ]

        threadDelay viewsCommitDelay
  where
    runDBIO :: YesodPersistBackend App (ResourceT IO) a -> IO a
    runDBIO f = runResourceT $ runPool (persistConfig app) f (connPool app)

-- | Forks the view cache daemon on a new thread and returns its 'ThreadId'.
forkViewsDaemon :: App -> IO ThreadId
forkViewsDaemon = forkIO . viewsDaemon

-- | Streams the content of a file over HTTP.
getDownloadR :: Text -> Handler ()
getDownloadR hmac = do
    mRequestType <- parseType <$> lookupGetParam "type"
    when (isNothing mRequestType)
        notFound
    let Just requestType = mRequestType

    app <- getYesod

    (file, uploadId, upload, path, h) <- runDB $ do
        Entity uploadId upload <- getBy404 $ UniqueHmac hmac

        Just file <- get $ uploadFileId upload

        let dir = hashDir (uploadDir app) (T.unpack $ fileSha1 file)
            path = getPath dir requestType

        -- Opens the file inside the transaction to ensure data consistency.
        h <- lift $ safeOpenFile path

        return (file, uploadId, upload, path, h)

    -- Updates the upload view count and the last view date.
    when (requestType == Original) $
        liftIO $ viewsCache app `uploadView` uploadId

    bs <- liftIO $ B.hGetContents h -- hGetContents closes the handle.
    allowGzip <- getGzipClientSupport

    (bs', size) <- case (requestType, fileCompressed file) of
        (Original, Just compressedSize) 
            | allowGzip -> do
                setHeader "Content-Encoding" "gzip"
                return (bs, compressedSize)
            | otherwise -> do
                return (decompress bs, fileSize file)
        (Original, Nothing) -> do
            return (bs, fileSize file)
        (_, _) -> do
            s <- liftIO $ hFileSize h
            return (bs, word64 s)

    setHeader "Cache-Control" "public, max-age=31536000"
    setHeader "Expires" "Thu, 31 Dec 2037 23:55:55 GMT"

    let mime = requestMime requestType (uploadMime upload)
        builder = fromLazyByteString bs'
        size' = Just $ int size
    sendResponse (mime, ContentBuilder builder size')
  where
    -- Parses the file type from the URL parameter.
    parseType Nothing            = Just Original
    parseType (Just "")          = Just Original
    parseType (Just "miniature") = Just Miniature
    parseType (Just "awebm")     = Just WebMAudio
    parseType (Just "mp3")       = Just MP3
    parseType (Just "vwebm")     = Just WebMVideo
    parseType (Just "mkv")       = Just MKV
    parseType (Just "png")       = Just PNG
    parseType _                  = Nothing

    -- Tries to open the file. 404 Not found if doesn't exists.
    safeOpenFile path = do
        eH <- liftIO $ E.try (openBinaryFile path ReadMode)
        case eH of
            Right h                     -> return h
            Left (_ :: E.SomeException) -> notFound

    -- Returns true if the browser supports the gzip encoding.
    getGzipClientSupport = do
        request <- waiRequest
        let headers = requestHeaders request
        case "accept-encoding" `lookup` headers of
            Just values ->
                let encodings = splitCommas $ C.unpack values
                in return $ "gzip" `elem` encodings
            Nothing -> return False
 
    requestMime Original  originalMime = C.pack $ T.unpack originalMime
    requestMime Miniature _            = typePng
    requestMime WebMAudio _            = "audio/webm"
    requestMime MP3       _            = "audio/mpeg"
    requestMime WebMVideo _            = "video/webm"
    requestMime MKV       _            = "video/x-matroska"
    requestMime PNG       _            = typePng

-- | Returns the URL to the given file for its specified type.
routeType :: Upload -> ObjectType -> Handler Text
routeType upload objType = do
    urlRdr <- getUrlRenderParams <*> pure (DownloadR (uploadHmac upload))
    return $ case objType of
        Original  -> urlRdr []
        Miniature -> urlRdr [("type", "miniature")]
        WebMAudio -> urlRdr [("type", "awebm")]
        MP3       -> urlRdr [("type", "mp3")]
        WebMVideo -> urlRdr [("type", "vwebm")]
        MKV       -> urlRdr [("type", "mkv")]
        PNG       -> urlRdr [("type", "png")]

-- | Splits a string on commas and removes spaces.
splitCommas :: String -> [String]
splitCommas [] = []
splitCommas xs =
    let (ys, zs) = break (== ',') xs
    in ys : splitCommas (dropWhile (== ' ') $ drop 1 zs)

int :: Integral a => a -> Int
int = fromIntegral

word64 :: Integral a => a -> Word64
word64 = fromIntegral
