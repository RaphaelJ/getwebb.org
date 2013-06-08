-- | Uses the JobDaemon to resizes images which are too large to be easily
-- displayed in a browser.
module JobsDaemon.ResizeImage (
      DisplayType (..) {- from Model -}, maxImageSize, displayable
    -- * Job
    , jobResize
    -- * Background compression queue management
    , putFile
    ) where

import Import

import qualified Vision.Image as I
import qualified Vision.Primitive as I

import JobsDaemon.Util (registerJob, runDBIO)

-- | The maximum size of an image to be displayed in the viewer.
maxImageSize :: I.Size
maxW, maxH :: Int
maxImageSize@(I.Size maxW maxH) = I.Size 2048 1200

-- | Returns an image which has been scaled down if its size is larger than
-- 'maxImageSize'
displayable :: I.RGBImage -> I.RGBImage
displayable img | w > maxW || h > maxH =
    I.resize I.NearestNeighbor img size'
  where
    I.Size w h = I.getSize img
    maxRatio = max (w % maxW) (h % maxH)
    size' = I.Size (round $ (w % 1) / maxRatio) (round $ (h % 1) / maxRatio)
displayable img                        = img

-- | Generates a resized image in background. Doesn't save the displayable
-- file if this one is larger than the original file.
jobResize :: App -> FileId -> DisplayType -> IO ()
jobResize app fileId destType = do
    Just file <- runDBIO app $ get fileId

    let dir = uploadDir app (fileHash file)
        path = getPath dir Original
        destPath = getPath dir (Display destType)

    img <- I.load path

    I.save (displayable img) destPath -- Will fail if the file has been removed.
    newSize <- getFileSize destPath

    if newSize < fileSize file
        then runDBIO app $
                updateWhere [ImageAttrsFile ==. fileId]
                            [ImageAttrsDisplayable =. Just destType]
        else removeFile destPath

-- | Adds a file to the background resizing queue.
putFile :: App -> FileId -> DisplayType -> [JobId] -> IO JobId
putFile app fileId destType deps = registerJob app fileId (Resize destType) deps
                                               (jobResize app fileId destType)
