{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Streams files to clients. Use a daemon to buffer statistics and save disk
-- accesses.
module Handler.Download (
    -- * Page handler
      getDownloadR
    -- * Transmission utilities
    , trackedBS, lazyBSToSource
    -- * URL utilities
    , routeType
    -- * Views daemon buffer management
    , ViewsBuffer, ViewsBufferEntry, viewsCommitDelay, newBuffer
    , incrementViewCount, addBandwidth, getBufferEntry
    -- * Starting the daemon
    , viewsDaemon, forkViewsDaemon
    ) where

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
import qualified Data.ByteString.Lazy.Internal as L
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Text as T
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import System.IO (Handle, IOMode (..), openBinaryFile, hFileSize, hClose)

import Blaze.ByteString.Builder (Builder, fromByteString)
import qualified Codec.Archive.Zip as Z
import Codec.Compression.GZip (decompress)
import Control.Monad.Trans.Resource (ResourceT)
import Data.Conduit (Source, Flush (Chunk), addCleanup, yield)
import Network.Mime (defaultMimeLookup)
import Network.Wai (requestHeaders)
import System.IO.Unsafe (unsafeInterleaveIO)

import JobsDaemon (runDBIO)
import Utils.Hmac (splitHmacs)
import Utils.Path (hashDir, getPath)

import System.TimeIt

-- | Streams the content of a file over HTTP.
getDownloadR :: Text -> Handler ()
getDownloadR hmacs' = do
    -- TODO: Return a 304 status if the browser sends us a if-modified-since
    -- header.

    case hmacs of
        [] -> notFound
        [hmac] -> do
            query <- parseQuery
            case query of
                Just (Display _)               -> streamDisplayable hmac
                Just (CompressedFile fileHmac) -> 
                    streamArchiveFile hmac fileHmac
                Just requestType               -> streamFile hmac requestType
                Nothing                        -> notFound
        (_:_) -> streamFiles
  where
    hmacs = splitHmacs hmacs'

    -- Streams a simple file to the client.
    streamFile hmac requestType = do
        (file, uploadId, upload, h) <- runDB $ do
            Entity uploadId upload <- getBy404 $ UniqueUploadHmac hmac
            Just file <- get $ uploadFileId upload

            -- Opens the file inside the transaction to ensure data consistency.
            h <- lift $ safeOpenFile file requestType

            return (file, uploadId, upload, h)

        bs' <- liftIO $ L.hGetContents h
        allowGzip <- getGzipClientSupport

        -- Doesn't decompress a compressed file if the client's browser supports
        -- Gzip.
        (bs, size) <- case (requestType, fileCompressed file) of
            (Original, Just compressedSize)
                | allowGzip -> do
                    setHeader "Content-Encoding" "gzip"
                    return (bs', compressedSize)
                | otherwise -> do
                    return (decompress bs', fileSize file)
            (Original, Nothing) -> do
                return (bs', fileSize file)
            (_, _) -> do
                size <- liftIO $ hFileSize h
                return (bs', word64 size)

        trackedBs <- getTrackedBS uploadId bs

        let mime = requestMime requestType (uploadName upload)
        streamByteString [h] trackedBs mime (Just size)

    -- Streams a displayable image to the client.
    streamDisplayable hmac = do
        (uploadId, upload, h, displayType) <- runDB $ do
            Entity uploadId upload <- getBy404 $ UniqueUploadHmac hmac

            let fileId = uploadFileId upload
            Just file <- get fileId
            when (fileType file /= Image) $
                lift notFound

            Just (Entity _ attrs)  <- getBy $ UniqueImageAttrs fileId

            let mDisplayType = imageAttrsDisplayable attrs
            when (isNothing mDisplayType) $
                lift notFound

            let Just displayType = mDisplayType

            -- Opens the file inside the transaction to ensure data consistency.
            h <- lift $ safeOpenFile file (Display displayType)

            return (uploadId, upload, h, displayType)

        bs <- liftIO (L.hGetContents h) >>= getTrackedBS uploadId
        size <- liftIO $ word64 <$> hFileSize h

        let mime = requestMime (Display displayType) (uploadName upload)
        streamByteString [h] bs mime (Just size)

    -- Decompresses and streams a file inside an archive to the client.
    streamArchiveFile hmac fileHmac = do
        (file, uploadId, archiveFile, h) <- runDB $ do
            Entity uploadId upload <- getBy404 $ UniqueUploadHmac hmac
            Entity _ archiveFile <- getBy404 $ UniqueArchiveFileHmac fileHmac

            when (archiveFileFileId archiveFile /= uploadFileId upload) $
                lift notFound

            Just file <- get $ uploadFileId upload

            -- Opens the file inside the transaction to ensure data consistency.
            h <- lift $ safeOpenFile file Original

            return (file, uploadId, archiveFile, h)

        fileBs'<- liftIO $ L.hGetContents h

        let fileBs = if isJust (fileCompressed file)
                then decompress fileBs'
                else fileBs'
            path = archiveFilePath archiveFile
            Just entry = Z.findEntryByPath (T.unpack path) $ Z.toArchive fileBs
            mime = defaultMimeLookup path
            size = word64 $ Z.eUncompressedSize entry

        bs <- getTrackedBS uploadId (Z.fromEntry entry)
        streamByteString [h] bs mime (Just size)

    -- Streams a set of file inside a .zip archive.
    streamFiles = do
        uploads' <- runDB $ do
            forM hmacs $ \hmac -> do
                -- Opens every file, skips non-existing/removed files.
                mUpload <- getBy $ UniqueUploadHmac hmac
                case mUpload of
                    Just entity@(Entity _ upload) -> do
                        Just file <- get $ uploadFileId upload

                        -- Opens the file inside the transaction to ensure data
                        -- consistency.
                        h <- lift $ safeOpenFile file Original

                        return $! Just (entity, file, h)
                    Nothing ->
                        return Nothing
        let uploads = take 25 {-FIXME: Space leak-} $ catMaybes $ uploads'

        when (null uploads)
            notFound

        archive <- foldM addToArchive Z.emptyArchive uploads
        let hs = map (\(_, _, h) -> h) uploads
            bs = Z.fromArchive archive

        streamByteString hs bs "application/zip" Nothing

    -- Returns the original archive with the upload's file appended.
    addToArchive archive (Entity uploadId upload, file, h) = do
        let name = T.unpack $ uploadName upload
            epoch = round $ utcTimeToPOSIXSeconds $ uploadUploaded upload
            decompress' = if isJust (fileCompressed file)
                              then decompress
                              else id

        bs <- liftIO (L.hGetContents h) >>= getTrackedBS uploadId . decompress'
        return $! Z.addEntryToArchive (Z.toEntry name epoch bs) archive

    -- Responds to the client with the ByteString content and closes the
    -- handlers afterwards.
    streamByteString :: [Handle] -> L.ByteString -> C.ByteString -> Maybe Word64
                     -> Handler ()
    streamByteString hs bs mime mSize = do
        setHeader "Buffer-Control" "public, max-age=31536000"
        setHeader "Expires" "Thu, 31 Dec 2037 23:55:55 GMT"

        whenJust mSize $ \size ->
            setHeader "Content-Length" (T.pack $ show size)

        let source = lazyBSToSource (mapM_ hClose hs) bs
        sendResponse (mime, ContentSource source)

    -- | Wraps the original ByteString so each transmitted chunk size is
    -- commited to the upload's total amount of transferred bytes.
    -- Updates the upload's last view after the stream has been fully streamed.
    getTrackedBS uploadId bs = do
        app <- getYesod
        let bwTracker = addBandwidth app uploadId
        liftIO $ trackedBS bwTracker (incrementViewCount app uploadId) bs

    -- Returns the type of the requested item or Nothing of the type is invalid.
    parseQuery = do
        mArchive <- lookupGetParam "archive"
        case mArchive of
            Just fileHmac -> return $! Just (CompressedFile fileHmac)
            Nothing       -> parseType <$> lookupGetParam "type"

    -- Parses the file type from the URL parameter.
    parseType Nothing            = Just Original
    parseType (Just "")          = Just Original
    parseType (Just "miniature") = Just Miniature
    parseType (Just "awebm")     = Just WebMAudio
    parseType (Just "mp3")       = Just MP3
    parseType (Just "vwebm")     = Just WebMVideo
    parseType (Just "mkv")       = Just MKV
    parseType (Just "display")   = Just (Display undefined)
    parseType _                  = Nothing

    -- Tries to open the file. 404 Not found if doesn't exists.
    safeOpenFile file requestType = do
        app <- getYesod
        let dir = hashDir app (T.unpack $ fileHash file)
            path = getPath dir requestType

        eH <- liftIO $ E.try (openBinaryFile path ReadMode)
        case eH of
            Right h                     -> return h
            Left (_ :: E.SomeException) -> notFound

    -- Returns true if the browser supports the gzip encoding.
    getGzipClientSupport = do
        headers <- requestHeaders <$> waiRequest
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
    requestMime (Display PNG)  _        = typePng
    requestMime (Display JPG)  _        = typeJpeg
    requestMime (Display GIF)  _        = typeGif
    requestMime _              _        = undefined

    -- Splits a string on commas and removes spaces.
    splitCommas :: String -> [String]
    splitCommas [] = []
    splitCommas xs =
        let (ys, zs) = break (== ',') xs
        in ys : splitCommas (dropWhile (== ' ') $ drop 1 zs)

-- | Wraps a 'L.ByteString' so each generated 'L.Chunk' size is commited to the
-- given action. Executes the finalizer when the full 'L.ByteString' has been
-- consumed.
trackedBS :: (Word64 -> IO ()) -> IO () -> L.ByteString -> IO L.ByteString
trackedBS bwTracker finalizer =
    lazyGo . L.toChunks
  where
    lazyGo = unsafeInterleaveIO . go

    go []     = finalizer >> return L.Empty
    go (b:bs) = do
        bwTracker $ word64 $ S.length b
        bs' <- lazyGo bs
        return $! L.Chunk b bs'

-- Creates a source which seeds the given 'L.ByteString' and which executes the
-- given finalizer when the source has been closed.
lazyBSToSource :: IO () -> L.ByteString -> Source (ResourceT IO) (Flush Builder)
lazyBSToSource finalizer =
    addCleanup finalizer' . go . L.toChunks
  where
    go []     = return ()
    go (b:bs) = do
        yield $ Chunk $ fromByteString b
        go bs

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
        Display _           -> [("type", "display")]
        CompressedFile hmac -> [("archive", hmac)]
  where
    urlRdr' = urlRdr (DownloadR (uploadHmac upload))

-- -----------------------------------------------------------------------------

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
incrementViewCount :: App -> UploadId -> IO ()
incrementViewCount app uploadId = do
    let (buffer, signal) = viewsBuffer app

    -- Updates the buffer atomically.
    currentTime <- getCurrentTime
    modifyMVar_ buffer $ \accum -> do
        let f _ (oldCount, _, bw) = (oldCount + 1, Just currentTime, bw)
        return $! M.insertWith f uploadId (1, Just currentTime, 0) accum

    -- Signals to the daemon that at least an entry has been added to the 
    -- buffer.
    _ <- signal `tryPutMVar` ()
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
        timeIt $ runDBIO app $ do
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

-- | Forks the view buffer daemon on a new thread and returns its) 'ThreadId'.
forkViewsDaemon :: App -> IO ThreadId
forkViewsDaemon = forkIO . viewsDaemon
