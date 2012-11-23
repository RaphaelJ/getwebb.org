{-# LANGUAGE OverloadedStrings #-}
-- | Recognises images and create miniature to them.
module Upload.Image (processImage, miniature, extensions, miniatureSize)
    where

import Import

import qualified Control.Exception as C
import qualified Data.Set as S
import Data.Text (pack)
import System.FilePath

import Vision.Image
import Vision.Primitive
import Graphics.Exif (fromFile, allTags)
import Graphics.Exif.Internal (tagFromName, tagTitle)

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
            let path = dir </> "original"
            -- Try to open the file as an image.
            mImg <- liftIO $ C.catch (load path)
                                     (const $ return Nothing)

            case mImg of
                Just img -> do
                    -- Creates the miniature and save it as "miniature.png".
                    -- Update the database row so the file type is Image and
                    -- adds possible EXIF tags.
                    let miniImg = miniature img
                    liftIO $ save miniImg (dir </> "miniature" <.> "png")

                    tags <- liftIO $ exifTags path

                    runDB $ do
                        update fileId [FileType =. Image]

                        forM_ tags $ \(title, value) ->
                            insert $ FileTag fileId title value

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

-- | Reads EXIF tags from the image file. Returns an empty list if the image
-- doesn't support EXIF tags.
exifTags :: FilePath -> IO [(Text, Text)]
exifTags path = do
    C.handle (return []) $ do
        tags <- fromFile path >>= allTags
        forM tags $ \(name, value) ->
            title <- tagFromName name >>= tagTitle
            return (pack title, pack value)

int :: Integral a => a -> Int
int = fromIntegral