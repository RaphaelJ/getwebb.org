-- | Functions which are useful to communicate with the FFmpeg command.
module Upload.FFmpeg (executable, )
    where

import System.Process (runInteractiveProcess)

executable :: FilePath
executable = "/usr/bin/ffmpeg"

runInteractiveProcess executable 

ffmpeg -i pipe:0
ffmpeg -i pipe:1
