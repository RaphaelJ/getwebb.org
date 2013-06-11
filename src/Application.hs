{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application
    ( makeApplication
    , getApplicationDev
    , makeFoundation
    ) where

import Import
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L

import Yesod.Default.Config (
      AppConfig, ConfigSettings (..), DefaultEnv (..)
    , appExtra, appEnv, configSettings, withYamlEnvironment
    )
import qualified Yesod.Default.Config
import Yesod.Default.Main
import Yesod.Default.Handlers
import Control.Monad.Logger (runLoggingT)
import Database.Persist.Sql (runMigration)
import Network.Wai.Middleware.Autohead (autohead)
import Network.Wai.Middleware.RequestLogger
import Network.HTTP.Conduit (newManager, def)
import System.Directory (doesFileExist)
import System.IO (stdout)
import System.Log.FastLogger (mkLogger)
import Web.ClientSession (randomKey)

import Settings

-- Import all relevant handler modules here.
-- Don't forget to add new modules to your cabal file!
import Account
import Handler.Comment
import Handler.Download
import qualified Handler.Download as D
import Handler.History
import Handler.Home
import Handler.Upload
import Handler.View

import qualified JobsDaemon.Daemon as J
import qualified JobsDaemon.Restore as J
import qualified JobsDaemon.Util as J

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
    _ <- J.forkJobsDaemon nJobsThreads foundation

    -- Initialize the logging middleware
    logWare <- mkRequestLogger def {
          outputFormat = if development
                then Detailed True
                else if extraReverseProxy $ appExtra $ settings foundation
                        then Apache FromHeader
                        else Apache FromSocket
        , destination = Logger $ appLogger foundation
        }

    app <- (logWare . autohead) <$> toWaiAppPlain foundation
    return $ logWare app

-- | Loads up any necessary settings, creates your foundation datatype, and
-- performs some initialization.
makeFoundation :: AppConfig DefaultEnv Extra -> IO App
makeFoundation conf = do
    manager <- newManager def

    s <- staticSite
    account <- makeAccount

    dbconf <- withYamlEnvironment "config/sqlite.yml" (appEnv conf)
              loadConfig >>= applyEnv
    p <- createPoolConfig (dbconf :: Settings.PersistConf)
    logger <- mkLogger True stdout
    key <- getEncryptionKey

    -- Initialises the concurrent queues.
    jQueue <- J.newQueue
    vBuffer <- D.newBuffer

    let foundation = App conf s account p manager dbconf logger key jQueue
                         vBuffer

    -- Performs database migrations using our application's logging settings.
    let migrations = migrateAccount >> migrateAll
    runLoggingT (runPool dbconf (runMigration migrations) p)
                (messageLoggerSource foundation logger)

    J.restoreJobQueue foundation

    return foundation

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

-- for yesod devel
getApplicationDev :: IO (Int, Application)
getApplicationDev =
    defaultDevelApp loader makeApplication
  where
    loader = Yesod.Default.Config.loadConfig (configSettings Development) {
          csParseExtra = parseExtra
        }
