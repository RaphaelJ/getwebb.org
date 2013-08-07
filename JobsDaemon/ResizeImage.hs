{-# LANGUAGE BangPatterns #-}
-- | Uses the JobDaemon to resizes images which are too large to be easily
-- displayed in a browser or in a social media.
module JobsDaemon.ResizeImage (
      ImageType (..) {- from Model -}, maxCardSize, maxDisplayableSize
    , isLarger, resize
    -- * Job
    , jobResize
    -- * Background compression queue management
    , putFile
    ) where

import Import
import Data.Ratio
import System.Directory (removeFile)

import Vision.Image (
      D, DIM2, F, Image (..), InterpolMethod (..), RGBImage, Source, U, Z (..)
    , (:.) (..)
    , convert, extent, load, save
    )
import qualified Vision.Image as I (resize)

import JobsDaemon.Util (registerJob, runDBIO)
import Util.Path (ObjectType (..), uploadDir, getFileSize, getPath)

-- | The maximum size of an image to be used as a Twitter card.
maxCardSize :: DIM2
maxCardSize = Z :. 640 :. 640

-- | The maximum size of an image to be displayed in the viewer.
maxDisplayableSize :: DIM2
maxDisplayableSize = Z :. 1200 :. 2048

-- | Returns 'True' if the image is larger than the given size on at least one
-- dimension.
isLarger :: Source r (Channel RGBImage) => RGBImage r -> DIM2 -> Bool
img `isLarger` (Z :. maxH :. maxW) = let Z :. h :. w = extent img
                                     in w > maxW || h > maxH

-- | Returns an image which has been scaled down to the maximum given size.
-- Preserves the aspect-ratio.
resize :: Source r (Channel RGBImage) => RGBImage r -> DIM2 -> RGBImage U
resize !img !(Z :. maxH :. maxW) =
    I.resize img NearestNeighbor size'
  where
    !(Z :. h :. w) = extent img
    !maxRatio = max (w % maxW) (h % maxH)
    !size' = Z :. (round $ (h % 1) / maxRatio) :. (round $ (w % 1) / maxRatio)
{-# SPECIALIZE resize :: RGBImage D -> DIM2 -> RGBImage U #-}
{-# SPECIALIZE resize :: RGBImage F -> DIM2 -> RGBImage U #-}

-- | Generates a resized image in background. Doesn't save the resized file if 
-- this last is larger than the original file.
jobResize :: App -> FileId -> ResizeJobType -> ImageType -> IO ()
jobResize app fileId job destType = do
    Just file <- runDBIO app $ get fileId

    let (objType, maxSize, attrsField) = case job of
            ResizeCard    ->
                (Card, maxCardSize, ImageAttrsCard)
            ResizeDisplay ->
                (Display, maxDisplayableSize, ImageAttrsDisplayable)
        dir = uploadDir app (fileHash file)
        path = getPath dir Original
        destPath = getPath dir (objType destType)

    eImg <- load path
    case eImg of
        Right img -> do
            -- Will fail if the file has been removed.
            save destPath (resize (convert img :: RGBImage D) maxSize)
            newSize <- getFileSize destPath

            if newSize < fileSize file
                then runDBIO app $
                    updateWhere [ImageAttrsFile ==. fileId]
                                [attrsField =. Just destType]
                else removeFile destPath
        Left err -> error err

-- | Adds a file to the background resizing queue for cards and displayable
-- images.
putFile :: App -> FileId -> ResizeJobType -> ImageType -> [JobId] -> IO JobId
putFile app fileId job destType deps =
    registerJob app fileId (Resize job destType) deps
                (jobResize app fileId job destType)
