{-# LANGUAGE OverloadedStrings #-}
-- | Settings are centralized, as much as possible, into this file. This
-- includes database connection settings, static file locations, etc.
-- In addition, you can configure a number of different aspects of Yesod
-- by overriding methods in the Yesod typeclass. That instance is
-- declared in the Foundation.hs file.
module Settings where

import Prelude
import Data.Word
import Text.Shakespeare.Text (st)
import Language.Haskell.TH.Syntax
import Database.Persist.Sqlite (SqliteConf)
import Yesod.Default.Config
import Yesod.Default.Util
import Data.Text (Text)
import Data.Yaml
import Control.Applicative
import Settings.Development
import Data.Default (def)
import Text.Hamlet

-- | Which Persistent backend this site is using.
type PersistConf = SqliteConf

-- | The location of static files on your system. This is a file system
-- path.
staticDir :: FilePath
staticDir = "static"

-- | The base URL of static files.
staticRoot :: AppConfig DefaultEnv x -> Text
staticRoot conf = [st|#{appRoot conf}/#{staticDir}|]

-- | Settings for 'widgetFile', such as which template languages to support and
-- default Hamlet settings.
widgetFileSettings :: WidgetFileSettings
widgetFileSettings = def
    { wfsHamletSettings = defaultHamletSettings
        { hamletNewlines = AlwaysNewlines
        }
    }

-- The rest of this file contains settings which rarely need changing by a
-- user.

widgetFile :: String -> Q Exp
widgetFile = (if development then widgetFileReload
                             else widgetFileNoReload)
              widgetFileSettings

data Extra = Extra {
      extraReverseProxy     :: Bool,       extraAccountRoot      :: Maybe Text
    , extraStaticRoot       :: Maybe Text, extraAdmin            :: Text
    , extraAdminMail        :: Text,       extraUploadDir        :: FilePath
    , extraMaxFileSize      :: Word64,     extraMaxRequestSize   :: Word64
    , extraMaxDailyUploads  :: Int,        extraMaxDailySize     :: Word64
    , extraTimeout          :: Int,        extraJobsThreads      :: Int
    , extraAdsenseClient    :: Text,       extraAdsenseSlotLarge :: Text
    , extraAdsenseSlotSmall :: Text,       extraAnalytics        :: Text
    , extraFacebook         :: Text,       extraTwitter          :: Text
    } deriving Show

parseExtra :: DefaultEnv -> Object -> Parser Extra
parseExtra _ o = Extra
    <$> o .:  "reverseProxy"
    <*> o .:? "accountRoot"
    <*> o .:? "staticRoot"
    <*> o .:  "admin"
    <*> o .:  "adminMail"
    <*> o .:  "uploadDir"
    <*> o .:  "maxFileSize"
    <*> o .:  "maxRequestSize"
    <*> o .:  "maxDailyUploads"
    <*> o .:  "maxDailySize"
    <*> o .:  "timeout"
    <*> o .:  "jobsThreads"
    <*> o .:  "adsenseClient"
    <*> o .:  "adsenseSlotLarge"
    <*> o .:  "adsenseSlotSmall"
    <*> o .:  "analytics"
    <*> o .:  "facebook"
    <*> o .:  "twitter"
