-- | Recognises images and create miniature to them.
module Upload.Image (processImage, miniature, extensions, miniatureSize)
    where

import Import

import qualified Control.Exception as C
import qualified Data.Set as S

-- | Files extensions which are supported by the DevIL image library.
extensions :: S.Set Text
extensions = S.fromDistinctAscList [
      ".bmp", ".cut", ".dds", ".doom", ".exr", ".gif", ".hdr", ".ico"
    , ".jp2", ".jpeg", ".jpg", ".lbm", ".mdl", ".mng", ".pal", ".pbm", ".pcd"
    , ".pcx", ".pgm", ".pic", ".png", ".ppm", ".psd", ".psp", ".raw", ".sgi"
    , ".tga", ".tif", ".tiff"
    ]

-- | The size of miniatures in pixels (both in width and height).
miniatureSize = 200

processImage :: Text -> FileId -> FilePath -> Handler Bool
processImage extension fileId filePath = do
    if not (extension `S.member` extensions)
        then return False
        else
            C.catch (do
                
            )
            (const $ return False)
            
            
  
  
  [pathIn, pathOut, strSize] <- getArgs
    i <- load pathIn :: IO RGBImage

-- | Generates a miniature from the input image.
miniature :: RGBImage -> RGBImage
miniature img =
    
  where
    -- Crops the image in a square rectangle as large as the largest side of the
    -- image.
    cropped =
        if w > h
            then crop img (Rect ((w - h) `quot` 2) 0 h h)
            else crop img (Rect 0 ((h - w) `quot` 2) w w)

    -- Resizes the cropped image to a square of size miniatureSize.
    resized = 

    Size w h = getSize img

    let mini = resize cropped miniSize

--     let withBorder = fromFunction miniSize (drawBorder mini miniSize) :: RGBImage
    save mini pathOut

    return ()
  where
    drawBorder img (Size w h) p@(Point x y)
        | x == 0 || y == 0 || x == (w-1) || y == (h-1) = pix `pixApply` const 0
        | x == 1 || y == 1 || x == (w-2) || y == (h-2) = pix `pixApply` brighter
        | otherwise = pix
      where
        pix = getPixel img p
        brighter val = fromIntegral $ min 255 $ int val + 50

int :: Integral a => a -> Int
int = fromIntegral
