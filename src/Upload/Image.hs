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
import System.TimeIt (timeIt)

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
            liftIO $ putStrLn "Load original image:"
            eImg <- liftIO $ timeIt $ C.try (I.load path)

            case eImg of
                Right img -> do
                    -- Creates the miniature and save it as "miniature.png".
                    -- Update the database row so the file type is Image and
                    -- adds possible EXIF tags.
                    let I.Size w h = I.getSize img
                    liftIO $! putStrLn "Miniature: "
                    liftIO $! timeIt $ do
                        let miniImg = miniature img
                        I.save miniImg (miniatureFile (takeDirectory path))

                    liftIO $! putStrLn "Tags: "
                    tags <- liftIO $ timeIt $ exifTags path

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
    -- Crops the image in a square rectangle as large as the largest side of the
    -- image.
    if w > h
        then drawBorder $ resize $ I.crop img (I.Rect ((w - h) `quot` 2) 0 h h)
        else drawBorder $ resize $ I.crop img (I.Rect 0 ((h - w) `quot` 2) w w)
  where
    -- Resizes the cropped image to a square of miniatureSize.
    resize img' = I.resize I.NearestNeighbor img' miniSize
    {-# INLINE resize #-}

    -- Draw a bright border surrounded by a dark border.
    drawBorder img' = I.fromFunction miniSize (drawBorderStep img')
    {-# INLINE drawBorder #-}
    drawBorderStep img' p@(I.Point x y)
        | x == 0 || y == 0 || x == (miniatureSize-1) || y == (miniatureSize-1) =
            pix `I.pixApply` const 0
        | x == 1 || y == 1 || x == (miniatureSize-2) || y == (miniatureSize-2) =
            pix `I.pixApply` brighter
        | otherwise = pix
      where
        pix = img' `I.getPixel` p
        brighter val = fromIntegral $ min 255 $ int val + 50
    {-# INLINE drawBorderStep #-}

    I.Size w h = I.getSize img
    !miniSize = I.Size miniatureSize miniatureSize

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