{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Recognises images and create miniature to them.
module Upload.Image (
      extensions, displayable, miniatureSize, processImage, miniature, exifTags
    ) where

import Import

import qualified Control.Exception as E
import Control.Monad
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Word
import System.FilePath ((<.>), takeDirectory)

import qualified Vision.Image as I
import qualified Vision.Primitive as I
import Graphics.Exif (fromFile, allTags)
import Graphics.Exif.Internals (tagFromName, tagTitle)
import System.TimeIt (timeIt)

import qualified Upload.Compression as C
import Upload.Path (ObjectType (..), getPath)

-- | Files extensions which are supported by the DevIL image library.
extensions :: S.Set Text
extensions = S.fromDistinctAscList [
      ".bmp", ".cut", ".dds", ".doom", ".exr", ".gif", ".hdr", ".ico"
    , ".jp2", ".jpeg", ".jpg", ".lbm", ".mdl", ".mng", ".pal", ".pbm", ".pcd"
    , ".pcx", ".pgm", ".pic", ".png", ".ppm", ".psd", ".psp", ".raw", ".sgi"
    , ".tga", ".tif", ".tiff"
    ]

-- | Files extensions which can be displayed in a browser.
displayable :: S.Set Text
displayable = S.fromDistinctAscList [".gif", ".jpeg", ".jpg", ".png"]

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
            eImg <- liftIO $ timeIt $ E.try (I.load path)

            case eImg of
                Right img -> do
                    -- Creates the miniature and save it as "miniature.png".
                    let dir = takeDirectory path
                        I.Size w h = I.getSize img
                    liftIO $ putStrLn "Miniature: "
                    liftIO $ timeIt $ do
                        let miniImg = miniature img
                        I.save miniImg (getPath dir Miniature)

                    -- If the image isn't displayable in a browser, creates
                    -- an additional .PNG.
                    inBrowser <- if ext `S.member` displayable
                        then return True
                        else do
                            liftIO $ putStrLn "Displayable: "
                            liftIO $ timeIt $ I.save img (getPath dir PNG)
                            return False

                    -- Update the database row so the file type is Image and
                    -- adds possible EXIF tags.
                    liftIO $ putStrLn "Tags: "
                    tags <- liftIO $ timeIt $ exifTags path

                    runDB $ do
                        update fileId [FileType =. Image]

                        _ <- insert $ ImageAttrs {
                              imageAttrsFileId = fileId
                            , imageAttrsWidth = word32 w
                            , imageAttrsHeight = word32 h
                            , imageAttrsInBrowser = inBrowser
                            }

                        forM_ tags $ \(title, value) ->
                            insert $! ExifTag fileId title value

                    app <- getYesod
                    liftIO $ app `C.putFile` fileId
                    return True
                Left (_ :: E.SomeException) -> return False

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
    eTags <- E.try $ do
        tags <- fromFile path >>= allTags
        forM tags $ \(!name, !value) -> do
            title <- tagFromName name >>= tagTitle
            E.evaluate (T.pack title, T.pack value)

    case eTags of
        Right tags -> return tags
        Left (_ :: E.SomeException) -> return []

int :: Integral a => a -> Int
int = fromIntegral

word32 :: Integral a => a -> Word32
word32 = fromIntegral