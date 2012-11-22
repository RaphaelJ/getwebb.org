{-# LANGUAGE OverloadedStrings #-}
-- | Recognises images and create miniature to them.
module Upload.Image (processImage, miniature, extensions, miniatureSize)
    where

import Import

import qualified Control.Exception as C
import qualified Data.Set as S
import System.FilePath

import Vision.Image
import Vision.Primitive

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
processImage ext fileId dir = do
    if not (ext `S.member` extensions)
        then return False
        else do
            -- Try to open the file as an image.
            mImg <- liftIO $ C.catch (load (dir </> "original"))
                                     (const $ return Nothing)

            case mImg of
                Just img -> do
                    -- Creates the miniature and save it as "miniature.png".
                    -- Update the database row so the file type is Image.
                    let miniImg = miniature img
                    liftIO $ save miniImg (dir </> "miniature" <.> "png")
                    runDB $ update fileId [FileType =. Image]
                    return True
                Nothing  -> return False

-- | Generates a miniature from the input image.
miniature :: RGBImage -> RGBImage
miniature img =
    fromFunction miniSize drawBorder
  where
    -- Crops the image in a square rectangle as large as the largest side of the
    -- image.
    cropped | w > h     = crop img (Rect ((w - h) `quot` 2) 0 h h)
            | otherwise = crop img (Rect 0 ((h - w) `quot` 2) w w)

    -- Resizes the cropped image to a square of miniatureSize.
    resized = resize cropped miniSize

    -- Draw a bright border surrounded by a dark border.
    drawBorder p@(Point x y)
        | x == 0 || y == 0 || x == (w-1) || y == (h-1) = pix `pixApply` const 0
        | x == 1 || y == 1 || x == (w-2) || y == (h-2) = pix `pixApply` brighter
        | otherwise = pix
      where
        !pix = getPixel resized p
        brighter val = fromIntegral $ min 255 $ int val + 50

    Size w h = getSize img
    miniSize = Size miniatureSize miniatureSize

int :: Integral a => a -> Int
int = fromIntegral
