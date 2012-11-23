-- | Functions which are useful to communicate with the FFmpeg command.
module Upload.FFmpeg (executable, )
    where

import System.Process (runInteractiveProcess)

executable :: FilePath
executable = "/usr/bin/ffmpeg"

time ffmpeg -i sintel_trailer-1080p.mp4 -s hd720 -vpre libvpx-720p -deadline realtime -f webm -ab 112k -y test.webm

time ffmpeg -i sintel_trailer-1080p.mp4 -s hd720 -vcodec libx264 -preset veryfast -acodec libmp3lame -ab 112k -y test.mkv


ffmpeg -i /home/rapha/sintel_trailer-1080p.mp4 -s 1280x720 -vpre libvpx-720p -f webm -ab 128k -y test.webm
["-s", "hd720", "-vpre", "libvpx-720p", "-f", "webm", "-ab", "128k", "-y", "-i", "pipe:0" "pipe:1"]
runInteractiveProcess executable 
