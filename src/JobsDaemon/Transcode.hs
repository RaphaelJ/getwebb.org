-- | Uses the JobDaemon to transcode medias to HTML 5 audio/video.
module JobsDaemon.Transcode (
    -- * Job
      jobTranscode
    -- * Background compression queue management
    , putFile
    ) where

import Import
import Data.Conduit.Binary (sinkFile)
import System.Directory (removeFile)
import System.Exit (ExitCode (..))
import Text.Printf

import JobsDaemon.Util (registerJob, runDBIO)
import Util.FFmpeg (argsWebM, argsH264, argsWebMAudio, argsMP3, encode)
import Util.Path (ObjectType (..), uploadDir, getPath)

-- | Transcodes a file and update the corresponding database entry.
jobTranscode :: App -> FileId -> IO ()
jobTranscode app fileId = do
    Just file <- runDBIO app $ get fileId

    let dir = uploadDir app (fileHash file)
        getPath' = getPath dir
        path = getPath' Original

    case fileType file of
        Audio -> do
            _ <- encodeFile argsWebMAudio path (getPath' WebMAudio)
            _ <- encodeFile argsMP3       path (getPath' MP3)
            updateHtml5Encoded
            return ()
        Video -> do
            _ <- encodeFile argsWebM path (getPath' WebMVideo)
            _ <- encodeFile argsH264 path (getPath' MKV)
            updateHtml5Encoded
            return ()
        _     ->
            error "Invalid file type."
  where
    encodeFile args inPath outPath = do
        code <- encode args inPath (sinkFile outPath)

        -- Removes the output file if the encoding failed.
        case code of
            e@(ExitFailure _) -> do
                removeFile outPath
                error (printf "FFmpeg failed with \"%s\"" (show e))
            _             -> return ()

    updateHtml5Encoded = runDBIO app $
        updateWhere [MediaAttrsFile ==. fileId] [MediaAttrsTranscoded =. True]

-- | Adds a file to a background transcoding queue.
putFile :: App -> FileId -> IO JobId
putFile app fileId = registerJob app fileId Transcode []
                                 (jobTranscode app fileId)
