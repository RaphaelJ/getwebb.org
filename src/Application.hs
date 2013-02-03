{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application
    ( makeApplication
    , getApplicationDev
    , makeFoundation
    ) where

import Import
import Control.Monad
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L

import Yesod.Default.Config
import Yesod.Default.Main
import Yesod.Default.Handlers
import Database.Persist.GenericSql (runMigration)
import qualified Database.Persist.Store
import Network.Wai.Middleware.Autohead (autohead)
import Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import Network.HTTP.Conduit (newManager, def)
import System.Directory (doesFileExist)
import Web.ClientSession (randomKey)

import qualified JobsDaemon as J
import Settings

-- Import all relevant handler modules here.
-- Don't forget to add new modules to your cabal file!
import Handler.Home
import Handler.Download
import qualified Handler.Download as D
import Handler.Upload
import Handler.View

import qualified Upload.Compression as C
import qualified Upload.Image as I
import qualified Upload.Media as M

-- This line actually creates our YesodDispatch instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see the
-- comments there for more details.
mkYesodDispatch "App" resourcesApp

-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
makeApplication :: AppConfig DefaultEnv Extra -> IO Application
makeApplication conf = do
    foundation <- makeFoundation conf

    -- Starts the background processes.
    let nJobsThreads = extraJobsThreads $ appExtra conf
    replicateM_ nJobsThreads (J.forkJobsDaemon foundation)

    app <- autohead <$> toWaiAppPlain foundation
    return $ logWare app
  where
    logWare = if development then logStdoutDev
                             else logStdout

makeFoundation :: AppConfig DefaultEnv Extra -> IO App
makeFoundation conf = do
    manager <- newManager def
    s <- staticSite
    dbconf <- withYamlEnvironment "config/sqlite.yml" (appEnv conf)
              Database.Persist.Store.loadConfig >>=
              Database.Persist.Store.applyEnv
    p <- Database.Persist.Store.createPoolConfig (dbconf :: Settings.PersistConfig)
    Database.Persist.Store.runPool dbconf (runMigration migrateAll) p
    key <- getEncryptionKey

    -- Initialises the concurrent queues.
    jQueue <- J.newQueue
    vBuffer <- D.newBuffer

    let app = App conf s p manager dbconf key jQueue vBuffer

    restoreJobQueue app

    return app

-- | Try to read the key file or initialize it with a random key.
getEncryptionKey :: IO L.ByteString
getEncryptionKey = do
    exists <- doesFileExist encryptKeyFile
    if exists
        then L.readFile encryptKeyFile
        else do
            (bs, _) <- randomKey
            S.writeFile encryptKeyFile bs
            return $ L.fromStrict bs

-- | Reloads the saved state of the background processing queue.
restoreJobQueue :: App -> IO ()
restoreJobQueue app =
    J.runDBIO app $ do
        jobs <- selectList [JobCompleted ==. False] [Asc JobFileId]
        forM_ jobs $ \(Entity jobId job) -> do
            deps <- getDependencies jobId

            let fileId = jobFileId job
                typ = jobType job
            liftIO $ J.enqueueJob app jobId deps (action fileId typ)
  where
    getDependencies jobId =
        selectList [JobDependencyJobId ==. jobId] [] >>=
        return . map (jobDependencyDependency . entityVal)

    action fileId Compression  = C.compressFile app fileId
    action fileId Transcode    = M.transcodeFile app fileId
    action fileId (Resize typ) = I.jobResize typ app fileId
    action fileId ExifTags     = I.jobExifTags app fileId

-- for yesod devel
getApplicationDev :: IO (Int, Application)
getApplicationDev =
    defaultDevelApp loader makeApplication
  where
    loader = loadConfig (configSettings Development) {
          csParseExtra = parseExtra
        }
