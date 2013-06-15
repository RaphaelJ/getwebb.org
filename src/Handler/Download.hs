{-# LANGUAGE BangPatterns, OverloadedStrings, ScopedTypeVariables #-}
-- | Streams files to clients. Use a daemon to buffer statistics and save disk
-- accesses.
module Handler.Download (
    -- * Page handlers
      getDownloadR, getDownloadMiniatureR, getDownloadWebMAR, getDownloadMP3R
    , getDownloadWebMVR, getDownloadMKVR, getDownloadDisplayableR
    , getDownloadArchiveR
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
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import System.IO (Handle, IOMode (..), openBinaryFile, hFileSize, hClose)

import qualified Codec.Archive.Zip as Z
import Codec.Compression.GZip (decompress)
import Data.Conduit (addCleanup)
import Network.Mime (defaultMimeLookup)
import Network.Wai (requestHeaders)
import System.IO.Unsafe (unsafeInterleaveIO)

import JobsDaemon.Util (runDBIO)
import Util.Hmac (Hmac (..), Hmacs (..))
import Util.Path (ObjectType (..), uploadDir, getPath)

-- TODO: Return a 304 status if the browser sends us a if-modified-since
-- header.

-- Handlers --------------------------------------------------------------------

-- | Responds with the file content if only one upload is requested.
-- Responds with a zip archive containing each upload if a list of upload has
-- been requested.
getDownloadR :: Text -> Handler TypedContent
getDownloadR hmacs' =
    case fromPathPiece hmacs' of
        Just (Hmacs [hmac])      -> streamFile hmac
        Just (Hmacs hmacs@(_:_)) -> streamFiles hmacs
        _                        -> notFound
  where
    -- Streams a simple file to the client.
    streamFile hmac = do
        ((Entity uploadId upload), file, h) <- openUpload hmac Original

        bs' <- liftIO $ L.hGetContents h
        allowGzip <- getGzipClientSupport

        -- Doesn't decompress a compressed file if the client supports GZip.
        (bs, size) <- case fileCompressed file of
            Just compressedSize | allowGzip -> do
                    addHeader "Content-Encoding" "gzip"
                    return (bs', compressedSize)
                                | otherwise -> do
                    return (decompress bs', fileSize file)
            Nothing                         -> return (bs', fileSize file)

        trackedBs <- getTrackedBS uploadId bs

        let mime = defaultMimeLookup (uploadName upload)
        streamByteString [h] trackedBs mime (Just size)

    -- Streams a set of file inside a .zip archive.
    streamFiles hmacs = do
        uploads' <- runDB $ do
            -- The archive can't contains two files with the same name.
            -- Thus a mutable Map is used to count the number of occurrences of
            -- each name.
            ioNames <- liftIO $ newIORef (M.empty :: M.Map Text Int)

            forM hmacs $ \hmac -> do
                -- Opens every file, skips non-existing/removed files.
                mUpload <- getBy $ UniqueUploadHmac hmac
                case mUpload of
                    Just entity@(Entity _ upload) -> do
                        file <- getJust $ uploadFile upload

                        -- Adds a suffix to duplicates filenames.
                        names <- liftIO $ readIORef ioNames
                        let name' = uploadName upload
                            name = case name' `M.lookup` names of
                                Just n  ->
                                    name' <> "_" <> (T.pack (show (n + 1)))
                                Nothing -> name'
                            names' = M.insertWith (+) name' 1 names
                        liftIO $ writeIORef ioNames names'

                        -- Opens the file inside the transaction to ensure data
                        -- consistency.
                        h <- lift $ safeOpenFile file Original

                        return $! Just (name, entity, file, h)
                    Nothing ->
                        return Nothing

        let uploads = take 15 {- FIXME: Space leak -} $ catMaybes $ uploads'

        when (null uploads)
            notFound

        archive <- foldM addToArchive Z.emptyArchive uploads
        let hs = map (\(_, _, _, h) -> h) uploads
            bs = Z.fromArchive archive

        streamByteString hs bs "application/zip" Nothing

    -- Returns the original archive with the upload's file appended.
    addToArchive archive (name, Entity uploadId upload, file, h) = do
        let name' = T.unpack name
            epoch = round $ utcTimeToPOSIXSeconds $ uploadCreated upload
            decompress' = if isJust (fileCompressed file)
                              then decompress
                              else id

        bs <- liftIO (L.hGetContents h) >>= getTrackedBS uploadId . decompress'
        return $! Z.addEntryToArchive (Z.toEntry name' epoch bs) archive

    -- Returns true if the browser supports the gzip encoding.
    getGzipClientSupport = do
        headers <- requestHeaders <$> waiRequest
        case "accept-encoding" `lookup` headers of
            Just values ->
                let encodings = splitCommas $ C.unpack values
                in return $ "gzip" `elem` encodings
            Nothing -> return False

    -- Splits a string on commas and removes spaces.
    splitCommas :: String -> [String]
    splitCommas [] = []
    splitCommas xs = let (ys, zs) = break (== ',') xs
                     in ys : splitCommas (dropWhile (== ' ') $ drop 1 zs)

-- | Responds with the miniature of the upload.
getDownloadMiniatureR :: Hmac -> Handler TypedContent
getDownloadMiniatureR = streamRawUploadObject Miniature typePng

-- | Responds with the WebM audio file of the upload.
getDownloadWebMAR :: Hmac -> Handler TypedContent
getDownloadWebMAR = streamRawUploadObject WebMAudio "audio/webm"

-- | Responds with the MP3 file of the upload.
getDownloadMP3R :: Hmac -> Handler TypedContent
getDownloadMP3R = streamRawUploadObject MP3 "audio/mpeg"

-- | Responds with the WebM video file of the upload.
getDownloadWebMVR :: Hmac -> Handler TypedContent
getDownloadWebMVR = streamRawUploadObject WebMVideo "video/webm"

-- | Responds with the MKV video file of the upload.
getDownloadMKVR :: Hmac -> Handler TypedContent
getDownloadMKVR = streamRawUploadObject MKV "video/x-matroska"

-- | Responds with the displayable version of the file of the upload.
getDownloadDisplayableR :: Hmac -> Handler TypedContent
getDownloadDisplayableR hmac = do
    (uploadId, h, displayType) <- runDB $ do
        Entity uploadId upload <- getBy404 $ UniqueUploadHmac hmac

        let fileId = uploadFile upload
        file <- getJust fileId
        when (fileType file /= Image)
            notFound

        Just (Entity _ attrs) <- getBy $ UniqueImageAttrs fileId

        case imageAttrsDisplayable attrs of
            Just displayType -> do
                -- Opens the file inside the transaction to ensure data
                -- consistency.
                h <- lift $ safeOpenFile file (Display displayType)

                return (uploadId, h, displayType)
            Nothing -> notFound

    let mime = case displayType of PNG -> typePng
                                   JPG -> typeJpeg
                                   GIF -> typeGif
    streamFileHandle uploadId mime h

-- | Responds with a file contained in an archive of an upload.
getDownloadArchiveR :: Hmac -> Hmac -> Handler TypedContent
getDownloadArchiveR hmac archiveHmac = do
    (file, uploadId, archiveFile, h) <- runDB $ do
        Entity uploadId upload <- getBy404 $ UniqueUploadHmac hmac
        Entity _ archiveFile   <- getBy404 $ UniqueArchiveFileHmac archiveHmac

        when (archiveFileFile archiveFile /= uploadFile upload)
            notFound

        file <- getJust $ uploadFile upload

        -- Opens the file inside the transaction to ensure data consistency.
        h <- lift $ safeOpenFile file Original

        return (file, uploadId, archiveFile, h)

    fileBs'<- liftIO $ L.hGetContents h

    let fileBs = case fileCompressed file of Just _  -> decompress fileBs'
                                             Nothing -> fileBs'
        path = archiveFilePath archiveFile
        Just entry = Z.findEntryByPath (T.unpack path) $ Z.toArchive fileBs
        mime = defaultMimeLookup path
        size = word64 $ Z.eUncompressedSize entry

    bs <- getTrackedBS uploadId (Z.fromEntry entry)
    streamByteString [h] bs mime (Just size)

-- HTTP streaming --------------------------------------------------------------

streamRawUploadObject :: ObjectType -> ContentType -> Hmac
                      -> Handler TypedContent
streamRawUploadObject objType mime hmac = do
    (Entity uploadId _, _, h) <- openUpload hmac objType
    streamFileHandle uploadId mime h

streamFileHandle :: UploadId -> ContentType -> Handle -> Handler TypedContent
streamFileHandle uploadId mime h = do
    bs   <- liftIO (L.hGetContents h) >>= getTrackedBS uploadId
    size <- word64 <$> liftIO (hFileSize h)

    streamByteString [h] bs mime (Just size)

-- | Responds to the client with the ByteString content and closes the handlers
-- afterwards.
streamByteString :: [Handle] -> L.ByteString -> ContentType -> Maybe Word64
                    -> Handler TypedContent
streamByteString hs bs mime mSize = do
    neverExpires

    whenJust mSize $ \size ->
        addHeader "Content-Length" (T.pack $ show size)

    respondSource mime $ addCleanup finalizer $ sendChunkLBS bs
  where
    finalizer _ = liftIO $ mapM_ hClose hs

-- Utilities -------------------------------------------------------------------

-- | Searches the upload in the database. Returns and open the associated file.
-- Returns a 404 error if the upload doesn't exist.
openUpload :: Hmac -> ObjectType -> Handler (Entity Upload, File, Handle)
openUpload hmac objType = runDB $ do
    entity@(Entity _ upload) <- getBy404 $ UniqueUploadHmac hmac
    file                     <- getJust $ uploadFile upload

    -- Opens the file inside the transaction to ensure data consistency.
    h <- lift $ safeOpenFile file objType

    return (entity, file, h)
  where

-- Tries to open the file given its type (original, miniature ...).
-- 404 Not found if doesn't exists.
safeOpenFile :: File -> ObjectType -> Handler Handle
safeOpenFile file objType = do
    app <- getYesod
    let dir = uploadDir app (fileHash file)
        path = getPath dir objType

    eH <- liftIO $ E.try (openBinaryFile path ReadMode)
    case eH of
        Right h                     -> return h
        Left (_ :: E.SomeException) -> notFound

-- | Wraps the original ByteString so each transmitted chunk size is commited
-- to the upload's total amount of transferred bytes.
-- Updates the upload's last view after the stream has been fully streamed.
getTrackedBS :: UploadId -> L.ByteString -> Handler L.ByteString
getTrackedBS uploadId bs = do
    app <- getYesod
    let bwTracker = addBandwidth app uploadId
        finalizer = incrementViewCount app uploadId
    liftIO $ trackedBS bwTracker finalizer bs

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

-- -----------------------------------------------------------------------------

-- | Temporary buffer for the count of views, the last view and the bandwidth
-- which have not been commited yet to the database. The second MVar is used to
-- signal that at least a file view has been added to the buffer.
type ViewsBuffer = (MVar (M.Map UploadId ViewsBufferEntry), MVar ())
type ViewsBufferEntry = (Word64, Maybe UTCTime, Word64)

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
    time <- getCurrentTime
    modifyMVar_ buffer $ \accum -> do
        return $! M.insertWith f uploadId (1, Just time, 0) accum

    -- Signals to the daemon that at least an entry has been added to the 
    -- buffer.
    _ <- signal `tryPutMVar` ()
    return ()
  where
    f (_, time, _) (!c, _, !bw) = let !c' = c + 1
                                  in (c', time, bw)

-- | Adds the number of bytes to the uncommited amount of bandwidth.
addBandwidth :: App -> UploadId -> Word64 -> IO ()
addBandwidth app uploadId !len = do
    let (buffer, signal) = viewsBuffer app

    -- Updates the buffer atomically.
    liftIO $ modifyMVar_ buffer $ \accum -> do
        return $! M.insertWith f uploadId (0, Nothing, len) accum

    -- Signal to the daemon at least an entry has been added to the buffer.
    _ <- liftIO $ signal `tryPutMVar` ()
    return ()
  where
    f _ (!c, !time, !bw) = let !bw' = bw + len
                           in (c, time, bw')

-- | Returns the buffer entry for the given upload. 'Nothing' if the database is
-- up to date.
getBufferEntry :: UploadId -> Handler (Maybe ViewsBufferEntry)
getBufferEntry uploadId = do
    app <- getYesod
    let (buffer, _) = viewsBuffer app
    accum <- liftIO $ readMVar buffer
    return $! uploadId `M.lookup` accum

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
        runDBIO app $ do
            forM_ (M.assocs oldBuffer) $ \(uploadId, (views, mViewed, bw)) ->
                case mViewed of
                    Just viewed -> update uploadId [
                          UploadViews +=. views, UploadViewed =. viewed
                        , UploadBandwidth +=. bw
                        ]
                    Nothing -> update uploadId [
                         UploadViews +=. views, UploadBandwidth +=. bw
                        ]

        threadDelay viewsCommitDelay

-- | Forks the view buffer daemon on a new thread and returns its 'ThreadId'.
forkViewsDaemon :: App -> IO ThreadId
forkViewsDaemon = forkIO . viewsDaemon
