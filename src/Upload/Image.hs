{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Recognises images and create miniature to them.
module Upload.Image (
      extensions, miniatureSize, processImage, miniature, exifTags
    ) where

import Import

import qualified Control.Exception as C
import Control.Monad
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Word
import System.FilePath

import qualified Vision.Image as I
import qualified Vision.Primitive as I
import Graphics.Exif (fromFile, allTags)
import Graphics.Exif.Internals (tagFromName, tagTitle)

import Upload.Utils (miniatureFile)

-- | Files extensions which are supported by the DevIL image library.
extensions :: S.Set Text
extensions = S.fromDistinctAscList [
      ".bmp", ".cut", ".dds", ".doom", ".exr", ".gif", ".hdr", ".ico"
    , ".jp2", ".jpeg", ".jpg", ".lbm", ".mdl", ".mng", ".pal", ".pbm", ".pcd"
    , ".pcx", ".pgm", ".pic", ".png", ".ppm", ".psd", ".psp", ".raw", ".sgi"
    , ".tga", ".tif", ".tiff"
    ]

-- | The size of miniatures in pixels (both in width and height).
miniatureSize :: Int
miniatureSize = 200

-- | Try to open the image and to generate a miniature.
processImage :: FilePath -> Text -> FileId -> Handler Bool
processImage path ext fileId = do
    if not (ext `S.member` extensions)
        then return False
        else do
            -- Try to open the file as an image.
            eImg <- liftIO $ C.try (I.load path)

            case eImg of
                Right img -> do
                    -- Creates the miniature and save it as "miniature.png".
                    -- Update the database row so the file type is Image and
                    -- adds possible EXIF tags.
                    let I.Size w h = I.getSize img
                        miniImg = miniature img
                    liftIO $! I.save miniImg (miniatureFile (takeDirectory path))

                    tags <- liftIO $ exifTags path

                    runDB $ do
                        update fileId [FileType =. Image]

                        _ <- insert $! ImageAttrs fileId (word32 w) (word32 h)

                        forM_ tags $ \(title, value) ->
                            insert $! ExifTag fileId title value

                    return True
                Left (_ :: C.SomeException) -> return False

-- | Generates a miniature from the input image.
miniature :: I.RGBImage -> I.RGBImage
miniature img =
    I.fromFunction miniSize drawBorder
  where
    -- Crops the image in a square rectangle as large as the largest side of the
    -- image.
    cropped | w > h     = I.crop img (I.Rect ((w - h) `quot` 2) 0 h h)
            | otherwise = I.crop img (I.Rect 0 ((h - w) `quot` 2) w w)

    -- Resizes the cropped image to a square of miniatureSize.
    resized = I.resize cropped miniSize

    -- Draw a bright border surrounded by a dark border.
    drawBorder p@(I.Point x y)
        | x == 0 || y == 0 || x == (w-1) || y == (h-1) =
            pix `I.pixApply` const 0
        | x == 1 || y == 1 || x == (w-2) || y == (h-2) =
            pix `I.pixApply` brighter
        | otherwise = pix
      where
        !pix = I.getPixel resized p
        brighter val = fromIntegral $ min 255 $ int val + 50

    I.Size w h = I.getSize img
    miniSize = I.Size miniatureSize miniatureSize

-- | Reads EXIF tags from the image file. Returns an empty list if the image
-- doesn't support EXIF tags.
exifTags :: FilePath -> IO [(Text, Text)]
exifTags path = do
    eTags <- C.try $ do
        tags <- fromFile path >>= allTags
        forM tags $ \(!name, !value) -> do
            title <- tagFromName name >>= tagTitle
            C.evaluate (T.pack title, T.pack value)

    case eTags of
        Right tags -> return tags
        Left (_ :: C.SomeException) -> return []

int :: Integral a => a -> Int
int = fromIntegral

word32 :: Integral a => a -> Word32
word32 = fromIntegral