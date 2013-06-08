{-# LANGUAGE BangPatterns, OverloadedStrings, ScopedTypeVariables #-}
-- | Recognises images and create miniature for them.
module Handler.Upload.Image (
    -- * Constants
      extensions, miniatureSize, maxImageSize
    -- * Upload processing
    , processImage
    -- * Images processing
    , miniature
    ) where

import Import

import qualified Control.Exception as E
import Control.Monad
import Data.Maybe
import Data.Ratio
import qualified Data.Set as S
import qualified Data.Text as T
import System.Directory (removeFile)
import System.FilePath (takeDirectory)

import qualified Vision.Image as I
import qualified Vision.Primitive as I
import Graphics.Exif (fromFile, allTags)
import Graphics.Exif.Internals (tagFromName, tagTitle)
import System.TimeIt (timeIt)

import qualified JobsDaemon.Compression as C
import qualified JobsDaemon.ResizeImage as R
import qualified JobsDaemon.ExifTags    as EXIF
import JobsDaemon.Util (registerJob, runDBIO)
import Util.Path (uploadDir, getPath, getFileSize)

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
miniatureSize = 100

-- | Try to open the image and to generate a miniature.
processImage :: FilePath -> Text -> FileId -> Handler Bool
processImage path ext fileId | not (ext `S.member` extensions) = return False
                             | otherwise = do
    app <- getYesod

    -- Tries to open the file as an image.
    liftIO $ putStrLn "Load original image:"
    eImg <- liftIO $ timeIt $ E.try (I.load path)

    case eImg of
        Right img -> do
            -- Creates the miniature and saves it.
            let dir = takeDirectory path
                size@(I.Size w h) = I.getSize img
            liftIO $ putStrLn "Miniature: "
            liftIO $ timeIt $ do
                let miniImg = miniature img
                I.save miniImg (getPath dir Miniature)

            (displayType, mDisplayJobId) <- genDisplayable dir size img

            -- Starts a background job to seek the EXIF tags of the
            -- image.
            let exifJob = jobExifTags app fileId
            exifJobId <- liftIO $ EXIF.putFile app fileId

            _ <- runDB $ do
                update fileId [FileType =. Image]

                insert $ ImageAttrs {
                      imageAttrsFile = fileId
                    , imageAttrsWidth = word32 w
                    , imageAttrsHeight = word32 h
                    , imageAttrsDisplayable = displayType
                    }

            -- Compresses the file after the EXIF tags and the displayable image
            -- processes have been executed.
            let compressDeps = exifJobId : maybeToList mDisplayJobId
            _ <- liftIO $ C.putFile app fileId compressDeps

            return True
        Left (_ :: E.SomeException) -> return False
  where
    -- Generates displayable image immediately if the file format is not
    -- displayable in the browser or in background if the image is only too 
    -- large.
    -- Returns the displayable type of the (possibly) generated image and the
    -- 'JobId' of the registered resizing background job (if launched).
    genDisplayable dir size img
        | ext == ".png"                   = genDisplayableAsync size PNG
        | ext == ".jpg" || ext == ".jpeg" = genDisplayableAsync size JPG
        | ext == ".gif"                   = genDisplayableAsync size GIF
        | otherwise                       = do
            let img' = displayable img
            liftIO $ I.save img' (getPath dir (Display PNG))
            return (Just PNG, Nothing)

    -- Generates the displayable image in background if the image is larger
    -- than maxImageSize.
    genDisplayableAsync (I.Size w h) destType = do
        app <- getYesod
        if w > maxW || h > maxH
            then liftIO $ do
                jobId <- R.putFile app fileId destType []
                return (Nothing, Just jobId)
            else
                return (Nothing, Nothing)

    I.Size maxW maxH = R.maxImageSize

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
--     resize img' = I.resize I.NearestNeighbor img' miniSize
    resize img' = I.resize I.Bilinear img' miniSize
    {-# INLINE resize #-}

    -- Draw a bright border surrounded by a dark border.
    drawBorder img' = I.fromFunction miniSize (drawBorderStep img')
    {-# INLINE drawBorder #-}
    drawBorderStep img' p@(I.Point x y)
        | x == 0 || y == 0 || x == (miniatureSize-1) || y == (miniatureSize-1) =
            pix `I.pixApply` darker
        | x == 1 || y == 1 || x == (miniatureSize-2) || y == (miniatureSize-2) =
            pix `I.pixApply` brighter
        | otherwise = pix
      where
        pix = img' `I.getPixel` p
        darker val = fromIntegral $ max 0 $ int val - 50
        brighter val = fromIntegral $ min 255 $ int val + 50
    {-# INLINE drawBorderStep #-}

    I.Size w h = I.getSize img
    !miniSize = I.Size miniatureSize miniatureSize
