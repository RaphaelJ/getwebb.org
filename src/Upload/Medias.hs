-- | Recognises medias (audio and video),  collects informations and create
-- HTML 5 audio/videos in a separate thread.
module Upload.Medias (
    -- * Processing queue management
      MediasQueue, newQueue, putFile
    -- * Starting the daemon
    , mediasDaemon, forkMediasDaemon
    -- * Processing uploads
    ) where

import Import

import Control.Concurrent (ThreadId,  Chan,  forkIO,  newChan,  writeChan,  readChan)
import qualified Data.Set as S
import System.FilePath ((</>),  (<.>))
import System.IO (ReadMode,  openFile,  hClose)

import Control.Monad.Trans.Resource (runResourceT)
import Data.Conduit (($=))
import Data.Conduit.Binary (sourceHandle,  sinkFile)
import Data.Conduit.Zlib (gzip)

import Upload.FFmpeg (MediaInfo (..), Duration (..), encode, getInfo)

type MediasQueue = Chan FileId

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
    forever do
        -- Waits until an FileId has been inserted in the queue.
        fileId <- readChan $ mediasQueue app

        mFile <- runDBIO $
            return do
                file <- get fileId

                -- Opens the file inside the transaction to ensures data
                -- consistence.
                let hash = unpack $ fileSha1 file
                    path = hashPath (uploadDir app) hash </> ".original"

                handle <- openFile path ReadMode

                case fileCompressed file of
                    Just _ -> Just (file,  handle,  path,  sourceFile path $= gzip)
                    _      -> Just (file,  handle,  path,  sourceFile path)

        when (isJust mFile) $ runResourceT do
            -- Process the file if it still exists.
            let (file,  handle,  path,  source) = fromJust mFile
            register $ hClose handle

            case fileType file of
                Audio -> do
                    _ <- encodeFile argsWebMAudio source (path <.> "webm")
                    _ <- encodeFile argsMP3 source (path <.> "mp3")
                Video -> do
                    _ <- encodeFile argsWebM source (path <.> "webm")
                    _ <- encodeFile argsH264 source (path <.> "mkv")
                _     ->
                    error "Invalid file type."
  where
    runDBIO :: YesodPersistBackend App IO a -> IO a
    runDBIO f = runPool (persistConfig app) f (connPool app)

    encodeFile args source outPath = encode args source (sinkFile outPath)

-- | Forks the medias encoding daemon on a new thread and returns its 'ThreadId'.
forkMediasDaemon :: App -> IO ThreadId
forkMediasDaemon = forkIO . mediasDaemon

-- | Try to open the file as a media. Enqueue the file if 
processMedia :: FilePath -> FilePath -> Text -> FileId -> Handler Bool
processMedia dir path ext fileId = do
    if not (ext `S.member` extensions)
        then return False
        else do
            