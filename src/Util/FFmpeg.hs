-- | Wrapper over the FFmpeg command.
module Util.FFmpeg (
      MediaInfo (..), MediaDuration (..), FFmpegArgs
    , ffmpeg, ffprobe, argsProbe, argsWebM, argsH264, argsWebMAudio, argsMP3
    , withExec, encode, getInfo
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
import Data.Conduit (Sink, ($$))
import Data.Conduit.Binary (sourceHandle, sinkFile)
import Data.Conduit.List (consume)
import System.Process (ProcessHandle, runInteractiveProcess, waitForProcess)
import Text.Regex.Posix (MatchResult (..), (=~~), (=~))

type FFmpegArgs = [String]

data MediaInfo = MediaInfo { miType :: FileType, miDuration :: MediaDuration }
    deriving (Show)

data MediaDuration = MediaDuration {
      mdHours :: Int, mdMins :: Int, mdSecs :: Int, mdCentis :: Int
    }

instance Show MediaDuration where
    show (MediaDuration h m s c) = printf "%d:%d:%d.%d" h m s c

-- | The path to the ffmpeg executable.
ffmpeg :: FilePath
ffmpeg = "/usr/bin/ffmpeg"

-- | The path to the ffprobe executable.
ffprobe :: FilePath
ffprobe = "/usr/bin/ffprobe"

-- | Arguments to the ffprobe command which accepts a media on stdin and returns
-- media informations on stderr.
argsProbe :: FilePath -> FFmpegArgs
argsProbe path = [path]

-- | Arguments to the ffmpeg command which accepts a media on stdin and encode
-- the corresponding 720p WebM file on stdout.
argsWebM :: FilePath -> FFmpegArgs
argsWebM path = ["-i", path, "-s", "1280x720", "-vpre", "libvpx-720p", "-b:v"
    , "2M", "-deadline", "realtime", "-b:a", "196k", "-ac", "2", "-f", "webm"
    , "-y", "pipe:1"
    ]

-- | Arguments to the ffmpeg command which accepts a media on stdin and encode
-- the corresponding 720p h264 file on stdout.
argsH264 :: FilePath -> FFmpegArgs
argsH264 path = ["-i", path, "-s", "1280x720", "-vcodec", "libx264", "-preset"
    , "veryfast", "-b:v", "2M", "-acodec", "libmp3lame", "-b:a", "196k", "-ac"
    , "2", "-f", "matroska", "-y", "pipe:1"
    ]

-- | Arguments to the ffmpeg command which accepts a media on stdin and encode
-- the corresponding WebM audio file on stdout.
argsWebMAudio :: FilePath -> FFmpegArgs
argsWebMAudio path = ["-i", path, "-vn", "-b:a", "196k", "-ac", "2", "-f", "webm"
    , "-y", "pipe:1"
    ]

-- | Arguments to the ffmpeg command which accepts a media on stdin and encode
-- the corresponding MP3 file on stdout.
argsMP3 :: FilePath -> FFmpegArgs
argsMP3 path = ["-i", path, "-vn", "-acodec", "libmp3lame", "-b:a", "196k", "-ac"
    , "2", "-f", "mp3", "-y", "pipe:1"
    ]

-- | Runs a the command and gives its stdin, stdout, stderr and process handle,
-- respectively to an action. Waits for the termination of the process and
-- closes handles after the action has been executed. Returns the exit code of
-- the command.
withExec :: FilePath -> FFmpegArgs
         -> (Handle-> Handle -> Handle -> ProcessHandle -> ResourceT IO a)
         -> IO (ExitCode, a)
withExec exec args action = runResourceT $! do
    (hStdin, hStdout, hStderr, pid) <- liftIO $ runExec
    _ <- register $ hClose hStdin -- hClose two times has no effect.
    _ <- register $ hClose hStdout
    _ <- register $ hClose hStderr

    ret <- action hStdin hStdout hStderr pid

    code <- liftIO $! waitForProcess pid
    return (code, ret)
  where
    runExec = runInteractiveProcess exec args Nothing Nothing

-- | Pushs a 'Source' in the stdin of ffmpeg called with the given arguments and
-- fill a 'Sink' with its stdout.
encode :: (FilePath -> FFmpegArgs) -> FilePath
       -> Sink ByteString (ResourceT IO) () -> IO ExitCode
encode args path sink = do
    -- Runs ffmpeg and seeds its input with the source and seeks its output in
    -- the sink.
    putStrLn $ unwords $ args path

    (code, _) <- withExec ffmpeg (args path) $ \_ hStdout hStderr _ -> do
        _ <- resourceForkIO $ sourceHandle hStderr $$ sinkFile "/dev/null"
        sourceHandle hStdout $$ sink

    return code

-- | Returns the type and duration of a media given by the source.
-- Returns 'Nothing' if ffmpeg fails to open the file.
getInfo :: FilePath -> IO (Maybe MediaInfo)
getInfo path = do
    let args = argsProbe path
    (code, out) <- withExec ffprobe args $ \_ hStdout hStderr _ -> do
        _ <- resourceForkIO $ sourceHandle hStdout $$ sinkFile "/dev/null"
        outputBs <- sourceHandle hStderr $$ consume
        return $ C.unpack $ C.fromChunks outputBs

    case code of
        ExitSuccess -> do
            return $! do -- Maybe monad
                durationMatch <- out =~~ durationRegex
                let duration = toDuration durationMatch
                    audio = out =~ ("Stream #.*: Audio:" :: String)
                    video = out =~ ("Stream #.*: Video:" :: String)
                    mp3   = out =~ ("Input #0, mp3," :: String)

                case (video, audio) of
                    (True ,    _) 
                        | mp3       -> Just $ MediaInfo Audio duration
                        | otherwise -> Just $ MediaInfo Video duration
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
