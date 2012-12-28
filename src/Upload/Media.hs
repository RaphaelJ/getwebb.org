{-# LANGUAGE ScopedTypeVariables #-}
-- | Recognises medias (audio and video), collects information and create
-- HTML 5 audio/videos in a separate thread.
module Upload.Media (
    -- * Processing queue management
      MediasQueue, newQueue, putFile
    -- * Starting the daemon
    , mediasDaemon, forkMediasDaemon
    -- * Processing uploads
    , processMedia
    ) where

import Import

import Control.Concurrent (ThreadId, Chan, forkIO, newChan, writeChan, readChan)
import qualified Control.Exception as E
import Control.Monad
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Word
import System.Directory
import System.Exit (ExitCode (..))
import System.FilePath ((<.>), takeDirectory, takeFileName)
import System.IO (hClose)

import Control.Monad.Trans.Resource (ResourceT, runResourceT, register)
import Database.Persist.Store (runPool)
import qualified Data.Aeson as J
import Data.Conduit (($$+-))
import Data.Conduit.Binary (sinkFile, sinkHandle)
import qualified Data.HashMap.Strict as H
import qualified Data.Vector as V
import Network.HTTP.Conduit (http, parseUrl, responseBody)
import qualified Network.Lastfm as L
import qualified Network.Lastfm.JSON.Track as L
import qualified Sound.TagLib as ID3
import System.Posix.Files (createSymbolicLink, removeLink)
import qualified Vision.Image as I

import Upload.Path (newTmpFile)

import Debug.Trace
import System.TimeIt

import Upload.FFmpeg (
      MediaInfo (..), MediaDuration (..), argsWebM, argsH264, argsWebMAudio
    , argsMP3, encode, getInfo
    )
import qualified Upload.Compression as C
import Upload.Image (miniature)
import Upload.Path (ObjectType (..), hashDir, uploadDir, getPath)

type MediasQueue = Chan FileId

lastfmKey :: L.APIKey
lastfmKey = L.APIKey "d1731c5c052d5cde7c82d56388b5d64e"

-- | Initialises a new encoding queue to be inserted in the foundation type.
newQueue :: IO MediasQueue
newQueue = newChan

-- | Adds a file to the compression queue.
putFile :: App -> FileId -> IO ()
putFile = writeChan . mediasQueue

-- | Files extensions which are supported by ffmpeg.
extensions :: S.Set Text
extensions = S.fromDistinctAscList [
      ".3g2", ".3gp", ".4xm", ".aac", ".ac3", ".act", ".adf", ".adx", ".aea"
    , ".aiff", ".alaw", ".alsa", ".amr", ".anm", ".apc", ".ape", ".applehttp"
    , ".asf", ".ass", ".au", ".avi", ".avs", ".bethsoftvid", ".bfi", ".bin"
    , ".bink", ".bit", ".bmv", ".c93", ".caf", ".cavsvideo", ".cdg", ".cdxl"
    , ".daud", ".dfa", ".dirac", ".dnxhd", ".dsicin", ".dts", ".dv", ".dv1394"
    , ".dxa", ".ea", ".ea_cdata", ".eac3", ".f32be", ".f32le", ".f64be"
    , ".f64le", ".fbdev", ".ffm", ".ffmetadata", ".film_cpk", ".filmstrip"
    , ".flac", ".flic", ".flv", ".g722", ".g723_1", ".g729", ".gsm", ".gxf"
    , ".h261", ".h263", ".h264", ".hls", ".ico", ".idcin", ".idf", ".iff"
    , ".ilbc", ".image2", ".image2pipe", ".ingenient", ".ipmovie", ".iss"
    , ".iv8", ".ivf", ".jacosub", ".jv", ".latm", ".lavfi", ".lmlm4", ".loas"
    , ".lxf", ".m4a", ".m4v", ".matroska", ".mgsts", ".microdvd", ".mj2"
    , ".mjpeg", ".mkv", ".mlp", ".mm", ".mmf", ".mov", ".mp3", ".mp4", ".mpc"
    , ".mpc8", ".mpeg", ".mpegts", ".mpegtsraw", ".mpegvideo", ".msnwctcp"
    , ".mtv", ".mulaw", ".mvi", ".mxf", ".mxg", ".nc", ".nsv", ".nut", ".nuv"
    , ".ogg", ".ogv", ".oma", ".oss", ".paf", ".pmp", ".psxstr", ".pulse"
    , ".pva", ".qcp", ".r3d", ".rawvideo", ".realtext", ".rl2", ".rm", ".roq"
    , ".rpl", ".rso", ".rtp", ".rtsp", ".s16be", ".s16le", ".s24be", ".s24le"
    , ".s32be", ".s32le", ".s8", ".sami", ".sap", ".sbg", ".sdp", ".shn"
    , ".siff", ".smjpeg", ".smk", ".smush", ".sol", ".sox", ".spdif", ".srt"
    , ".subviewer", ".swf", ".thp", ".tiertexseq", ".tmv", ".truehd", ".tta"
    , ".tty", ".txd", ".u16be", ".u16le", ".u24be", ".u24le", ".u32be", ".u32le"
    , ".u8", ".v4l2", ".vc1", ".vc1test", ".video4linux2", ".vmd", ".voc"
    , ".vqf", ".w64", ".wav", ".wc3movie", ".webm", ".webvtt", ".wsaud"
    , ".wsvqa", ".wtv", ".wv", ".x11grab", ".xa", ".xbin", ".xmv", ".xwma"
    , ".yop", ".yuv4mpegpipe"
    ]

-- | Waits files to encode on the concurrent queue and process them. Never
-- returns.
mediasDaemon :: App -> IO ()
mediasDaemon app =
    forever $ do
        -- Waits until an FileId has been inserted in the queue.
        fileId <- readChan $ mediasQueue app

        mFile <- runDBIO $ get fileId

        when (isJust mFile) $ runResourceT $ do
            -- Process the file if it still exists.
            let file = fromJust mFile
                hash = T.unpack $ fileSha1 file
                dir = hashDir (uploadDir app) hash
                getPath' = getPath dir

            case fileType file of
                Audio -> do
                    _ <- encodeFile argsWebMAudio (getPath' "webm")
                    _ <- encodeFile argsMP3       (getPath' "mp3")
                    return ()
                Video -> do
                    _ <- encodeFile argsWebM (getPath' "webm")
                    _ <- encodeFile argsH264 (getPath' "mkv")
                    return ()
                _     ->
                    error "Invalid file type."

            liftIO $ app `C.putFile` fileId
  where
    runDBIO :: YesodPersistBackend App (ResourceT IO) a -> IO a
    runDBIO f = runResourceT $ runPool (persistConfig app) f (connPool app)

    encodeFile args outPath = do
        code <- liftIO $ encode args path (sinkFile outPath)

        -- Removes the ouput file if the encoding failed.
        case code of
            ExitFailure _ -> liftIO $ removeFile outPath
            _ -> return ()

-- | Forks the medias encoding daemon on a new thread and returns its
-- 'ThreadId'.
forkMediasDaemon :: App -> IO ThreadId
forkMediasDaemon = forkIO . mediasDaemon

-- | Try to open the file as a media. Enqueue the file for re-encoding.
processMedia :: FilePath -> Text -> FileId -> Handler Bool
processMedia path ext fileId = do
    if not (ext `S.member` extensions)
        then return False
        else do
            mInfo <- liftIO $ getInfo path
            liftIO $ print mInfo

            case mInfo of
                Just (MediaInfo mediaType duration) -> do
                    mAudioAttrs <- mp3Tags

                    runDB $ do
                        update fileId [FileType =. mediaType]

                        _ <- insert $ MediaAttrs fileId (toCentisec duration)

                        when (isJust mAudioAttrs) $ do
                            _ <- insert $! fromJust mAudioAttrs
                            return ()

                    -- Adds the media to the media re-encoding queue.
                    app <- getYesod
                    liftIO $ app `putFile` fileId

                    return True
                Nothing -> do
                    return False
  where
    toCentisec (MediaDuration h m s c) =
        word64 c + word64 s * 100 + word64 m * 60 * 100 + word64 h * 3600 * 100

    -- Retrieves the tags of MP3 tracks and their icons from Last.fm.
    mp3Tags :: Handler (Maybe AudioAttrs)
    mp3Tags | ext /= ".mp3" = return Nothing
            | otherwise     =
        runMaybeT $ do
            -- Runs the retrieving of the ID3 tags in the MaybeT monad.

            -- Creates a symbolic link to the file with an .mp3 extension 
            -- because taglib requires an .mp3 extension.
            let mp3Path = path <.> "mp3"
            liftIO $ createSymbolicLink (takeFileName path) mp3Path

            tagFile <- MaybeT $! liftIO $ ID3.open mp3Path
            liftIO $ removeLink mp3Path

            tag <- MaybeT $! liftIO $ ID3.tag tagFile

            mAlbum   <- liftIO $ maybeStrTag <$> ID3.album tag
            mArtist  <- liftIO $ maybeStrTag <$> ID3.artist tag
            mComment <- liftIO $ maybeStrTag <$> ID3.comment tag
            mGenre   <- liftIO $ maybeStrTag <$> ID3.genre tag
            mTitle   <- liftIO $ maybeStrTag <$> ID3.title tag
            mTrack   <- liftIO $ maybeIntTag <$> ID3.track tag
            mYear    <- liftIO $ maybeIntTag <$> ID3.year tag

            liftIO $ print [mAlbum, mArtist, mComment, mGenre, mTitle]
            liftIO $ print [mTrack, mYear]

            -- Ensures that at least one tag has been found.
            _ <- MaybeT $! return $! msum [
                  mAlbum, mArtist, mComment, mGenre, mTitle
                ]
            _ <- MaybeT $! return $! mTrack `mplus` mYear

            -- Tries to find information about the album from Last.fm.
            case (mArtist, mTitle) of
                (Just artist, Just title) -> do
                    mLast <- lift $ lastfm (T.unpack artist) (T.unpack title)
                    case mLast of
                        Just (url, boolMiniature) ->
                            return $! AudioAttrs fileId mAlbum mArtist mComment
                                                 mGenre mTitle mTrack mYear
                                                 (Just url) boolMiniature
                        _                         ->
                            return $! AudioAttrs fileId mAlbum mArtist mComment
                                                 mGenre mTitle mTrack mYear 
                                                 Nothing False
                _                         ->
                    return $! AudioAttrs fileId mAlbum mArtist mComment mGenre
                                         mTitle mTrack mYear Nothing False

    -- Returns Nothing if the tag's string is empty.
    maybeStrTag "" = Nothing
    maybeStrTag xs = trace xs $ Just (T.pack xs)

    -- Returns Nothing if the tag's value is zero.
    maybeIntTag 0 = Nothing
    maybeIntTag n = Just (int n)

    -- Returns the last.fm url about the track if it exists and 'True' if a
    -- miniature has been generated in a file named "miniature.png".
    lastfm :: String -> String -> Handler (Maybe (Text, Bool))
    lastfm artist title = do
        let search = Left (L.Artist artist, L.Track title)
            autocorrect = Just (L.Autocorrect True)
        response <- liftIO $ L.getInfo search autocorrect Nothing lastfmKey

        case parseLastfmResponse response of
            Nothing -> return Nothing
            Just (url, Nothing) -> return $! Just (url, False)
            Just (url, Just coverUrl) -> do
                app <- getYesod

                -- Download the cover image in a temporary file
                (tmp, hTmp) <- liftIO $ newTmpFile app "cover_"
                liftIO $ putStrLn "Lastfm:"
                request <- liftIO $ parseUrl $ T.unpack coverUrl
                liftIO $ timeIt $ runResourceT $ do
                    _ <- register $ hClose hTmp
                    imgResponse <- http request (httpManager app)
                    responseBody imgResponse $$+- sinkHandle hTmp

                -- Generates the cover image miniature.
                eImg <- liftIO $ E.try (I.load tmp)
                case eImg of
                    Right img -> do
                        liftIO $ I.save (miniature img) miniaturePath
                        liftIO $ removeFile tmp
                        return $! Just (url, True)
                    Left (_ :: E.SomeException) -> do
                        liftIO $ removeFile tmp
                        return $! Just (url, False)

    -- Parses the last.fm JSON response and return the possible URL and the
    -- possible URL to the track's cover art.
    parseLastfmResponse :: Either L.LastfmError L.Response
                        -> Maybe (Text, Maybe Text)
    parseLastfmResponse (Left _)   = Nothing
    parseLastfmResponse (Right bs) = do
        -- Runs the parsing of the JSON object in the Maybe monad.
        J.Object obj <- J.decode' bs

        J.Object track <- "track" `H.lookup` obj
        J.String url <- "url" `H.lookup` track

        let mCoverUrl = do
            J.Object album <- "album" `H.lookup` track
            J.Array imgs <- "image" `H.lookup` album

            -- Takes the largest image.
            msum [
                  imgs `imgExtract` "extralarge"
                , imgs `imgExtract` "large", imgs `imgExtract` "medium"
                , imgs `imgExtract` "small"
                ]

        Just (url, mCoverUrl)

    -- Returns the url of the image from the given size if it exists, 'Nothing'
    -- otherwise.
    imgExtract :: J.Array -> Text -> Maybe Text
    imgs `imgExtract` size =
        let size' = Just $ J.String size
            cond ~(J.Object img) =
                size' == "size" `H.lookup` img
        in do
            J.Object img <- V.find cond imgs
            J.String imgUrl <- "#text" `H.lookup` img
            return imgUrl

    miniaturePath = miniatureFile (takeDirectory path)

int :: Integral a => a -> Int
int = fromIntegral
word64 :: Integral a => a -> Word64
word64 = fromIntegral