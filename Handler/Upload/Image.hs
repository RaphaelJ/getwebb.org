{-# LANGUAGE BangPatterns, OverloadedStrings, ScopedTypeVariables #-}
-- | Recognises images and create miniature for them.
module Handler.Upload.Image (
    -- * Constants
      extensions, miniatureSize
    -- * Upload processing
    , processImage
    -- * Images processing
    , miniature
    ) where

import Import

import qualified Control.Exception as E
import Data.Maybe
import qualified Data.Set as S
import System.FilePath (takeDirectory)

import qualified Vision.Image as I
import qualified Vision.Primitive as I

import qualified JobsDaemon.Compression as C
import qualified JobsDaemon.ResizeImage as R
import qualified JobsDaemon.ExifTags    as EXIF
import Util.Path (ObjectType (..), getPath)

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
miniatureSize = 150

-- | Specifies the way the displayable image is generated.
data DisplayableGen = SyncDisplayableGen  ImageType
                    | AsyncDisplayableGen JobId
                    | NoDisplayable

-- | Try to open the image and to generate a miniature.
processImage :: FilePath -> Text -> FileId -> Handler Bool
processImage path ext fileId | not (ext `S.member` extensions) = return False
                             | otherwise = do
    app <- getYesod

    -- Tries to open the file as an image.
    eImg <- liftIO $ E.try (I.load path)

    case eImg of
        Right img -> do
            -- Creates the miniature and saves it.
            let dir = takeDirectory path
                I.Size w h = I.getSize img
            liftIO $ do
                let miniImg = miniature img
                I.save miniImg (getPath dir Miniature)

            -- Creates a resized image for social medias.
            mCardJobId <- genCard img

            -- Creates a potential scaled down image for the site's viewer.
            displayableGen <- genDisplayable dir img
            let displayType = case displayableGen of
                    SyncDisplayableGen destType -> Just destType
                    _                           -> Nothing

            -- Starts a background job to seek the EXIF tags of the
            -- image.
            exifJobId <- liftIO $ EXIF.putFile app fileId []

            _ <- runDB $ do
                update fileId [FileType =. Image]

                insert $ ImageAttrs {
                      imageAttrsFile        = fileId
                    , imageAttrsWidth       = word32 w
                    , imageAttrsHeight      = word32 h
                    , imageAttrsCard        = Nothing
                    , imageAttrsDisplayable = displayType
                    }

            -- Compresses the file once the EXIF tags and the secondary images
            -- processes have been executed.
            let mDisplayableJobId = case displayableGen of
                    AsyncDisplayableGen displayJobId -> Just displayJobId
                    _                                -> Nothing
                compressDeps = exifJobId : catMaybes [ mCardJobId
                                                     , mDisplayableJobId ]
            _ <- liftIO $ C.putFile app fileId compressDeps

            return True
        Left (_ :: E.SomeException) -> return False
  where
    -- Generates a scaled down image for social medias if the image is too
    -- large to be used directly as a card.
    genCard img | img `R.isLarger` R.maxCardSize = do
                    app <- getYesod
                    let destType = fromMaybe PNG imgType
                    liftIO $ Just <$> R.putFile app fileId ResizeCard destType 
                                                []
                | otherwise = return Nothing

    -- Generates displayable image immediately if the file format is not
    -- displayable in the browser or in background if the image is only too
    -- large.
    genDisplayable dir img =
        case imgType of
            Just destType -> genDisplayableAsync img destType
            Nothing       -> do
                let img' = R.resize img R.maxDisplayableSize
                liftIO $ I.save img' (getPath dir (Display PNG))
                return $ SyncDisplayableGen PNG

    -- Generates the displayable image in background if the image is larger
    -- than maxDisplayableSize.
    genDisplayableAsync img destType
        | img `R.isLarger` R.maxDisplayableSize = do
            app <- getYesod
            jobId <- liftIO $ R.putFile app fileId ResizeDisplay destType []
            return $ AsyncDisplayableGen jobId
        | otherwise = return NoDisplayable

    -- Nothing for non displayable images.
    imgType | ext == ".png"                   = Just PNG
            | ext == ".jpg" || ext == ".jpeg" = Just JPG
            | ext == ".gif"                   = Just GIF
            | otherwise                       = Nothing

-- | Generates a miniature from the input image.
miniature :: I.RGBImage -> I.RGBImage
miniature img =
    -- Crops the image in a square rectangle as large as the largest side of the
    -- image.
    if w > h then resize $ I.crop img (I.Rect ((w - h) `quot` 2) 0 h h)
             else resize $ I.crop img (I.Rect 0 ((h - w) `quot` 2) w w)
  where
    -- Resizes the cropped image to a square of miniatureSize.
--     resize img' = I.resize I.NearestNeighbor img' miniSize
    resize img' = I.resize I.Bilinear img' miniSize
    {-# INLINE resize #-}

--     -- Draw a bright border surrounded by a dark border.
--     drawBorder img' = I.fromFunction miniSize (drawBorderStep img')
--     {-# INLINE drawBorder #-}
--     drawBorderStep img' p@(I.Point x y)
--         | x == 0 || y == 0 || x == (miniatureSize-1) || y == (miniatureSize-1) =
--             pix `I.pixApply` darker
--         | x == 1 || y == 1 || x == (miniatureSize-2) || y == (miniatureSize-2) =
--             pix `I.pixApply` brighter
--         | otherwise = pix
--       where
--         pix = img' `I.getPixel` p
--         darker val = fromIntegral $ max 0 $ int val - 50
--         brighter val = fromIntegral $ min 255 $ int val + 50
--     {-# INLINE drawBorderStep #-}

    I.Size w h = I.getSize img
    !miniSize = I.Size miniatureSize miniatureSize
