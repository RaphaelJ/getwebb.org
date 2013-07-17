{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
-- | Streams files to clients. Use a daemon to buffer statistics and save disk
-- accesses.
module Handler.Download (
    -- * Page handlers
      getDownloadR
    , getDownloadMiniatureR, getDownloadCardR, getDownloadDisplayableR
    , getDownloadWebMAR, getDownloadMP3R, getDownloadWebMVR, getDownloadMKVR
    , getDownloadArchiveR
    ) where

import Import

import qualified Control.Exception as E
import Control.Monad
import qualified Control.Monad.Trans.State as S
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import System.IO (Handle, IOMode (..), openBinaryFile, hFileSize, hClose)

import qualified Codec.Archive.Zip as Z
import Codec.Compression.GZip (decompress)
import Data.Conduit (addCleanup)
import Network.Mime (defaultMimeLookup)
import Network.Wai (requestHeaders)

import Handler.Download.ViewsCache (getTrackedBS)
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
        uploads' <- runDB $ flip S.evalStateT (M.empty :: M.Map Text Int) $ do
            -- The archive can't contains two files with the same name.
            -- Uses a StateT to store a Map to count the number of occurrences
            -- of each name.

            forM hmacs $ \hmac -> do
                -- Opens every file, skips non-existing/removed files.
                mUpload <- getBy $ UniqueUploadHmac hmac
                case mUpload of
                    Just entity@(Entity _ upload) -> do
                        file <- getJust $ uploadFile upload

                        -- Adds a suffix to duplicates filenames.
                        names <- S.get
                        let name' = uploadName upload
                            name = case name' `M.lookup` names of
                                Just n  ->
                                    name' <> "_" <> (T.pack (show (n + 1)))
                                Nothing -> name'
                        S.put $ M.insertWith (+) name' 1 names

                        -- Opens the file inside the transaction to ensure data
                        -- consistency.
                        h <- lift $ lift $ safeOpenFile file Original

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

-- | Responds with a smaller image for social medias.
getDownloadCardR :: Hmac -> Handler TypedContent
getDownloadCardR = streamResizedImage imageAttrsCard Card

-- | Responds with the displayable version of the file of the upload.
getDownloadDisplayableR :: Hmac -> Handler TypedContent
getDownloadDisplayableR = streamResizedImage imageAttrsDisplayable Display

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

-- | Streams a card or a displayable image if it has been generated.
streamResizedImage :: (ImageAttrs -> Maybe ImageType)
                   -> (ImageType -> ObjectType) -> Hmac -> Handler TypedContent
streamResizedImage attrGetter typeConstr hmac = do
    (uploadId, h, displayType) <- runDB $ do
        Entity uploadId upload <- getBy404 $ UniqueUploadHmac hmac

        let fileId = uploadFile upload
        file <- getJust fileId
        when (fileType file /= Image)
            notFound

        Just (Entity _ attrs) <- getBy $ UniqueImageAttrs fileId

        case attrGetter attrs of
            Just displayType -> do
                -- Opens the file inside the transaction to ensure data
                -- consistency.
                h <- lift $ safeOpenFile file (typeConstr displayType)

                return (uploadId, h, displayType)
            Nothing -> notFound

    let mime = case displayType of PNG -> typePng
                                   JPG -> typeJpeg
                                   GIF -> typeGif
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

