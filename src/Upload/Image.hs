module Upload.Image (extensions)
    where

import Import

import qualified Data.Set as S

extensions = fromDistinctAscList [
      ".bmp", ".cut", ".dds", ".doom", ".exr", ".gif", ".hdr", ".ico"
    , ".jp2", ".jpg", ".lbm", ".mdl", ".mng", ".pal", ".pbm", ".pcd", ".pcx"
    , ".pgm", ".pic", ".png", ".ppm", ".psd", ".psp", ".raw", ".sgi", ".tga"
    , ".tif"
    ]
