-- | Functions which are useful to communicate with the FFmpeg command.
module Upload.FFmpeg (
      MediaInfo (..), MediaDuration (..), FFmpegArgs
    , executable, argsDuration, argsWebM, argsH264, argsWebMAudio, argsMP3
    , withFFmpeg, encode, getInfo
    ) where

import Import

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy.Char8 as C
import System.Exit (ExitCode (..))
import System.IO (Handle, hClose)
import Text.Printf (printf)

import Control.Monad.Trans.Resource (
      ResourceT, runResourceT, resourceForkIO, register
    )
import Data.Conduit (Source, Sink, ($$))
import Data.Conduit.Binary (sourceHandle, sinkHandle)
import Data.Conduit.List (consume)
import System.Process (ProcessHandle, runInteractiveProcess, waitForProcess)
import Text.Regex.Posix (MatchResult (..), (=~~), (=~))

type FFmpegArgs = [String]

data MediaInfo = MediaInfo { miType :: FileType, miDuration :: MediaDuration }

data MediaDuration = MediaDuration {
      dHours :: Int, dMins :: Int, dSecs :: Int, dCenti :: Int
    }

instance Show MediaDuration where
    show (MediaDuration h m s c) = printf "%d:%d:%d.%d" h m s c

-- | The path to the ffmpeg executable.
executable :: FilePath
executable = "/usr/bin/ffmpeg"

-- | Arguments to the ffmpeg command which accepts a media on stdin and returns
-- media informations on stderr.
argsDuration :: FFmpegArgs
argsDuration = ["-i", "pipe:0"]

-- | Arguments to the ffmpeg command which accepts a media on stdin and encode
-- the corresponding 720p WebM file on stdout.
argsWebM :: FFmpegArgs
argsWebM = ["-i", "pipe:0", "-s", "1280x720", "-vpre", "libvpx-720p", "-b:v"
    , "2M", "-deadline", "realtime", "-b:a", "196k", "-ac", "2", "-f", "webm"
    , "-y", "pipe:1"
    ]

-- | Arguments to the ffmpeg command which accepts a media on stdin and encode
-- the corresponding 720p h264 file on stdout.
argsH264 :: FFmpegArgs
argsH264 = ["-i", "pipe:0", "-s", "1280x720", "-vcodec", "libx264", "-preset"
    , "veryfast", "-b:v", "2M", "-acodec", "libmp3lame", "-b:a", "196k", "-ac"
    , "2", "-y", "pipe:1"
    ]

-- | Arguments to the ffmpeg command which accepts a media on stdin and encode
-- the corresponding WebM audio file on stdout.
argsWebMAudio :: FFmpegArgs
argsWebMAudio = ["-i", "pipe:0", "-vn", "-b:a", "196k", "-ac", "2", "-f", "webm"
    , "-y", "pipe:1"
    ]
-- | Arguments to the ffmpeg command which accepts a media on stdin and encode
-- the corresponding MP3 file on stdout.
argsMP3 :: FFmpegArgs
argsMP3 = ["-i", "pipe:0", "-vn", "-acodec", "libmp3lame", "-b:a", "196k", "-ac"
    , "2", "-f", "mp3", "-y", "pipe:1"
    ]

-- | Runs a the ffmpeg command and gives its stdin, stdout, stderr and process
-- handle, respectively to an action. Waits for the termination of the process
-- and closes handles after the action has been executed. Returns the exit code
-- of the command.
withFFmpeg :: FFmpegArgs
           -> (Handle-> Handle -> Handle -> ProcessHandle -> ResourceT IO a)
           -> IO (ExitCode, a)
withFFmpeg args action = runResourceT $! do
    (hStdin, hStdout, hStderr, pid) <- liftIO $ runFFmpeg
    _ <- register $ hClose hStdin -- hClose two times has no effect.
    _ <- register $ hClose hStdout
    _ <- register $ hClose hStderr

    ret <- action hStdin hStdout hStderr pid

    code <- liftIO $! waitForProcess pid
    return (code, ret)
  where
    runFFmpeg = runInteractiveProcess executable args Nothing Nothing

-- | Pushs a 'Source' in the stdin of ffmpeg called with the given arguments and
-- fill a 'Sink' with its stdout.
encode :: FFmpegArgs -> Source (ResourceT IO) ByteString
       -> Sink ByteString (ResourceT IO) () -> IO ExitCode
encode args source sink = do
    -- Runs ffmpeg and seeds its input with the source and seeks its output in
    -- the sink.
    (code, _) <- withFFmpeg args $ \hStdin hStdout _ _ -> do
        _ <- resourceForkIO $ sourceHandle hStdout $$ sink
        source $$ sinkHandle hStdin

    return code

-- | Returns the type and duration of a media given by the source.
-- Returns 'Nothing' if ffmpeg fails to open the file.
getInfo :: Source (ResourceT IO) ByteString -> IO (Maybe MediaInfo)
getInfo source = do
    (code, output) <- withFFmpeg argsDuration $ \hStdin _ hStderr _ -> do
        _ <- resourceForkIO $ source $$ sinkHandle hStdin
        outputBs <- sourceHandle hStderr $$ consume
        return $ C.unpack $ C.fromChunks outputBs

    case code of
        ExitSuccess ->
            return $! do -- Maybe monad
                durationMatch <- output =~~ durationRegex
                let duration = toDuration durationMatch
                    audio = output =~ ("Stream #.*: Audio:" :: String)
                    video = output =~ ("Stream #.*: Video:" :: String)

                case (video, audio) of
                    (True ,    _) -> Just $ MediaInfo Video duration
                    (False, True) -> Just $ MediaInfo Audio duration
                    _             -> Nothing
        ExitFailure _ ->
            return Nothing
  where
    durationRegex :: String
    durationRegex = "Duration: ([0-9]+):([0-9]+):([0-9]+).([0-9]+)"

    toDuration match = 
        let [h, m, s, c] = mrSubList match
        in MediaDuration (read h) (read m) (read s) (read c)
