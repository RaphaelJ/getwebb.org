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

import qualified Vision.Image as I
import qualified Vision.Primitive as I

import JobsDaemon.Util (registerJob, runDBIO)
import Util.Path (ObjectType (..), uploadDir, getFileSize, getPath)

-- | The maximum size of an image to be used as a Twitter card.
maxCardSize :: I.Size
maxCardSize = I.Size 640 640

-- | The maximum size of an image to be displayed in the viewer.
maxDisplayableSize :: I.Size
maxDisplayableSize = I.Size 2048 1200

-- | Returns 'True' if the image is larger than the given size on at least one
-- dimension.
isLarger :: I.RGBImage -> I.Size -> Bool
img `isLarger` I.Size maxW maxH = let I.Size w h = I.getSize img
                                  in w > maxW || h > maxH

-- | Returns an image which has been scaled down to the maximum given size.
-- Preserves the aspect-ratio.
resize :: I.RGBImage -> I.Size -> I.RGBImage
resize img maxSize@(I.Size maxW maxH) | img `isLarger` maxSize =
    I.resize I.NearestNeighbor img size'
  where
    I.Size w h = I.getSize img
    maxRatio = max (w % maxW) (h % maxH)
    size' = I.Size (round $ (w % 1) / maxRatio) (round $ (h % 1) / maxRatio)
resize img _                                         = img

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

    img <- I.load path

    -- Will fail if the file has been removed.
    I.save (resize img maxSize) destPath
    newSize <- getFileSize destPath

    if newSize < fileSize file
        then runDBIO app $
            updateWhere [ImageAttrsFile ==. fileId]
                        [attrsField =. Just destType]
        else removeFile destPath

-- | Adds a file to the background resizing queue for cards and displayable
-- images.
putFile :: App -> FileId -> ResizeJobType -> ImageType -> [JobId] -> IO JobId
putFile app fileId job destType deps =
    registerJob app fileId (Resize job destType) deps
                (jobResize app fileId job destType)
