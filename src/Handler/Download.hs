{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Handler.Download (
    -- * Views daemon buffer management
      ViewsBuffer, newBuffer, incrementViewCount, addBandwidth, getBufferEntry
    -- * Starting the daemon
    , viewsCommitDelay, viewsDaemon, forkViewsDaemon
    -- * Page handler
    , getDownloadR
    -- * Transmission utilities
    , lazyBSToSource
    -- * URL utilities
    , ObjectType (..), routeType
    )where

import Import

import Control.Concurrent (
      ThreadId, MVar, forkIO, newMVar, newEmptyMVar, readMVar, modifyMVar_
    , takeMVar, tryPutMVar, swapMVar, threadDelay
    )
import qualified Control.Exception as E
import Control.Monad
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Text as T
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Word
import System.IO (IOMode (..), openBinaryFile, hFileSize, hClose)

import Blaze.ByteString.Builder (Builder, fromByteString)
import qualified Codec.Archive.Zip as Z
import Codec.Compression.GZip (decompress)
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Data.Conduit (Source, Flush (Chunk), addCleanup, yield)
import Database.Persist.Store (runPool)
import Network.Mime (defaultMimeLookup)
import Network.Wai (requestHeaders)

import Upload.Path (ObjectType (..), hashDir, uploadDir, getPath)

import System.TimeIt

-- | Temporary buffer for the count of views, the last view and the bandwidth
-- which have not been commited yet to the database. The second MVar is used to
-- signal that at least a file view has been added to the buffer.
type ViewsBufferEntry = (Word64, Maybe UTCTime, Word64)
type ViewsBuffer = (MVar (M.Map UploadId ViewsBufferEntry), MVar ())

-- | Initialises a new view buffer to be inserted in the foundation type.
newBuffer :: IO ViewsBuffer
newBuffer = do
    buffer <- newMVar M.empty
    signal <- newEmptyMVar
    return (buffer, signal)

-- | Updates the buffer to increment by one the number of views and to update 
-- the last view date.
incrementViewCount :: UploadId -> Handler ()
incrementViewCount uploadId = do
    app <- getYesod
    let (buffer, signal) = viewsBuffer app

    -- Updates the buffer atomically.
    currentTime <- liftIO $ getCurrentTime
    liftIO $ modifyMVar_ buffer $ \accum -> do
        let f _ (oldCount, _, bw) = (oldCount + 1, Just currentTime, bw)
        return $! M.insertWith f uploadId (1, Just currentTime, 0) accum

    -- Signal to the daemon at least an entry has been added to the buffer.
    _ <- liftIO $ signal `tryPutMVar` ()
    return ()

-- | Adds the number of bytes to the uncommited amount of bandwidth.
addBandwidth :: App -> UploadId -> Word64 -> IO ()
addBandwidth app uploadId len = do
    let (buffer, signal) = viewsBuffer app

    -- Updates the buffer atomically.
    liftIO $ modifyMVar_ buffer $ \accum -> do
        let f _ (c, time, bw) = (c, time, bw + len)
        return $! M.insertWith f uploadId (0, Nothing, len) accum

    -- Signal to the daemon at least an entry has been added to the buffer.
    _ <- liftIO $ signal `tryPutMVar` ()
    return ()

-- | Returns the buffer entry for the given upload. 'Nothing' if the database is
-- up to date.
getBufferEntry :: UploadId -> Handler (Maybe ViewsBufferEntry)
getBufferEntry uploadId = do
    app <- getYesod
    let (buffer, _) = viewsBuffer app
    accum <- liftIO $ readMVar buffer
    return $ uploadId `M.lookup` accum

-- | Delay in microseconds at which the views buffer is commited to the 
-- database.
viewsCommitDelay :: Int
viewsCommitDelay = 5 * 10^(6 :: Int) -- Every five seconds.

-- | Accumulates the view count and the last view for each file to batch update
-- the database every few seconds (saves a lot of disk accesses).
viewsDaemon :: App -> IO ()
viewsDaemon app = do
    let (buffer, signal) = viewsBuffer app
    forever $ do
        -- Waits for at least a file view
        takeMVar signal

        -- Resets the buffer.
        oldBuffer <- buffer `swapMVar` M.empty

        -- Inserts the old buffer in the database.
        timeIt $ runDBIO $ do
            forM_ (M.assocs oldBuffer) $ \(uploadId, (views, mLastView, bw)) ->
                case mLastView of
                    Just lastView -> update uploadId [
                         UploadViews +=. views, UploadLastView =. lastView
                        , UploadBandwidth +=. bw
                        ]
                    Nothing -> update uploadId [
                         UploadViews +=. views, UploadBandwidth +=. bw
                        ]

        threadDelay viewsCommitDelay
  where
    runDBIO :: YesodPersistBackend App (ResourceT IO) a -> IO a
    runDBIO f = runResourceT $ runPool (persistConfig app) f (connPool app)

-- | Forks the view buffer daemon on a new thread and returns its) 'ThreadId'.
forkViewsDaemon :: App -> IO ThreadId
forkViewsDaemon = forkIO . viewsDaemon

-- | Streams the content of a file over HTTP.
getDownloadR :: Text -> Handler ()
getDownloadR hmac = do
    mArchive <- lookupGetParam "archive"
    case mArchive of
        Nothing       -> streamFile
        Just fileHmac -> streamArchiveFile fileHmac
  where
    -- Streams the file to the client.
    streamFile = do
        mRequestType <- parseType <$> lookupGetParam "type"
        when (isNothing mRequestType)
            notFound
        let Just requestType = mRequestType

        (file, uploadId, upload, h) <- runDB $ do
            Entity uploadId upload <- getBy404 $ UniqueUploadHmac hmac

            Just file <- get $ uploadFileId upload

            -- Opens the file inside the transaction to ensure data consistency.
            h <- lift $ safeOpenFile file requestType

            return (file, uploadId, upload, h)

        bs <- liftIO $ L.hGetContents h
        allowGzip <- getGzipClientSupport

        -- Doesn't decompress a compressed file if the client's browser supports
        -- Gzip.
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

        let mime = requestMime requestType (uploadName upload)
        streamByteString uploadId h bs' mime size

    -- Decompresses and streams a file inside an archive to the client.
    streamArchiveFile fileHmac = do
        (file, uploadId, archiveFile, h) <- runDB $ do
            Entity uploadId upload <- getBy404 $ UniqueUploadHmac hmac
            Entity _ archiveFile <- getBy404 $ UniqueArchiveFileHmac fileHmac

            when (archiveFileFileId archiveFile /= uploadFileId upload) $
                lift notFound

            Just file <- get $ uploadFileId upload

            -- Opens the file inside the transaction to ensure data consistency.
            h <- lift $ safeOpenFile file Original

            return (file, uploadId, archiveFile, h)

        fileBs <- liftIO $ L.hGetContents h

        let fileBs' = if isJust (fileCompressed file)
                then decompress fileBs
                else fileBs
            path = archiveFilePath archiveFile
            Just entry = Z.findEntryByPath (T.unpack path) $ Z.toArchive fileBs'
            bs = Z.fromEntry entry
            mime = defaultMimeLookup path
            size = Z.eUncompressedSize entry
        streamByteString uploadId h bs mime size

    -- Responds to the client with the ByteString content and closes the handler
    -- afterwards.
    streamByteString uploadId h bs mime size = do
        -- Updates the upload view count and the last view date.
        incrementViewCount uploadId

        setHeader "Buffer-Control" "public, max-age=31536000"
        setHeader "Expires" "Thu, 31 Dec 2037 23:55:55 GMT"
        setHeader "Content-Length" (T.pack $ show size)

        app <- getYesod
        let updateBandwidth = addBandwidth app uploadId
            source = lazyBSToSource updateBandwidth (hClose h) bs

        sendResponse (mime, ContentSource source)

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
    safeOpenFile file requestType = do
        app <- getYesod
        let dir = hashDir (uploadDir app) (T.unpack $ fileSha1 file)
            path = getPath dir requestType

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

    -- Returns the mimetype depending on the request type and the file
    -- extension.
    requestMime Original       filename = defaultMimeLookup filename
    requestMime Miniature      _        = typePng
    requestMime WebMAudio      _        = "audio/webm"
    requestMime MP3            _        = "audio/mpeg"
    requestMime WebMVideo      _        = "video/webm"
    requestMime MKV            _        = "video/x-matroska"
    requestMime PNG            _        = typePng
    requestMime _              _        = undefined

-- Creates a sources which commits the amount of transferred to the first
-- function while sending the file and executes the second action when the
-- source has been closed.
lazyBSToSource :: (Word64 -> IO ()) -> IO () -> L.ByteString
               -> Source (ResourceT IO) (Flush Builder)
lazyBSToSource updateBandwidth finalizer =
    addCleanup finalizer' . go . L.toChunks
  where
    go []     = return ()
    go (x:xs) = do
        yield $ Chunk $ fromByteString x
        liftIO $ updateBandwidth $ word64 $ S.length x
        go (xs)

    finalizer' _ = liftIO $ finalizer

-- | Returns the URL to the given file for its specified type.
routeType :: (Route App -> [(Text, Text)] -> Text) -> Upload -> ObjectType
          -> Text
routeType urlRdr upload obj =
    urlRdr' $ case obj of
        Original            -> []
        Miniature           -> [("type", "miniature")]
        WebMAudio           -> [("type", "awebm")]
        MP3                 -> [("type", "mp3")]
        WebMVideo           -> [("type", "vwebm")]
        MKV                 -> [("type", "mkv")]
        PNG                 -> [("type", "png")]
        CompressedFile hmac -> [("archive", hmac)]
  where
    urlRdr' = urlRdr (DownloadR (uploadHmac upload))

-- | Splits a string on commas and removes spaces.
splitCommas :: String -> [String]
splitCommas [] = []
splitCommas xs =
    let (ys, zs) = break (== ',') xs
    in ys : splitCommas (dropWhile (== ' ') $ drop 1 zs)

word64 :: Integral a => a -> Word64
word64 = fromIntegral
