module JobsDaemon.Restore (restoreJobQueue) where

import Import

import Control.Monad

import qualified JobsDaemon.Compression as C
import qualified JobsDaemon.ExifTags    as E
import qualified JobsDaemon.ResizeImage as R
import qualified JobsDaemon.Transcode   as T
import JobsDaemon.Util (enqueueJob, runDBIO)

-- | Reloads the saved state of the processing queue from the database.
restoreJobQueue :: App -> IO ()
restoreJobQueue app = runDBIO app $ do
    jobs <- selectList [JobCompleted ==. False] [Asc JobId]
    forM_ jobs $ \(Entity jobId job) -> do
        deps <- getDependencies jobId

        let fileId = jobFile job
            typ = jobType job
        liftIO $ enqueueJob app jobId deps (action fileId typ)
  where
    getDependencies jobId = do
        deps <- selectList [JobDependencyJob ==. jobId]
                           [Asc JobDependencyDependency]
        return $ map (jobDependencyDependency . entityVal) deps

    action fileId Compression       = C.jobCompress  app fileId
    action fileId ExifTags          = E.jobExifTags  app fileId
    action fileId (Resize destType) = R.jobResize    app fileId destType
    action fileId Transcode         = T.jobTranscode app fileId
