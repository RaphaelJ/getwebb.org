{-# LANGUAGE OverloadedStrings #-}
module Foundation where

import Prelude
import Control.Concurrent.Chan (Chan)
import Data.Text (pack, unpack)
import Yesod
import Yesod.Static
import Yesod.Default.Config
import Yesod.Default.Util (addStaticContentExternal)
import Network.HTTP.Conduit (Manager)
import qualified Settings
import Settings.Development (development)
import qualified Database.Persist.Store
import Database.Persist.GenericSql
import Settings (widgetFile, Extra (..))
import Text.Jasmine (minifym)
import Text.Hamlet (hamletFile)
import Web.ClientSession (getKey)

import Model

-- | The site argument for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App {
      settings :: AppConfig DefaultEnv Extra
    , getStatic :: Static -- ^ Settings for static file serving.
    , connPool :: Database.Persist.Store.PersistConfigPool Settings.PersistConfig -- ^ Database connection pool.
    , httpManager :: Manager
    , persistConfig :: Settings.PersistConfig
    , compressionQueue :: Chan FileId
    , mediasQueue :: Chan FileId
    }

-- Set up i18n messages. See the message folder.
mkMessage "App" "messages" "en"

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/handler
--
-- This function does three things:
--
-- * Creates the route datatype AppRoute. Every valid URL in your
--   application can be represented as a value of this type.
-- * Creates the associated type:
--       type instance Route App = AppRoute
-- * Creates the value resourcesApp which contains information on the
--   resources declared below. This is used in Handler.hs by the call to
--   mkYesodDispatch
--
-- What this function does *not* do is create a YesodSite instance for
-- App. Creating that instance requires all of the handler functions
-- for our application to be in scope. However, the handler functions
-- usually require access to the AppRoute datatype. Therefore, we
-- split these actions into two functions and place them in separate files.
mkYesodData "App" $(parseRoutesFile "config/routes")

type Form x = Html -> MForm App App (FormResult x, Widget)

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    approot = ApprootMaster $ appRoot . settings

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is two year.
    makeSessionBackend _ = do
        key <- getKey "config/client_session_key.aes"
        return . Just $ clientSessionBackend key (2 * 365 * 24 * 60)

    defaultLayout widget = do
        master <- getYesod
        mmsg <- getMessage

        mAdminKey <- tryAdminKey
        countHistory <- case mAdminKey of 
            Just adminKey -> runDB $ count [UploadAdminKey ==. adminKey]
            Nothing -> return 0

        pc <- widgetToPageContent $ do
            $(widgetFile "normalize")
            $(widgetFile "default-layout-style")
            $(widgetFile "default-layout-header")
            $(widgetFile "default-layout-body")
--             $(widgetFile "default-layout-footer")
        hamletToRepHtml $(hamletFile "templates/default-layout.hamlet")

    -- This is done to provide an optimization for serving static files from
    -- a separate domain. Please see the staticRoot setting in Settings.hs
--     urlRenderOverride y (StaticR s) =
--         Just $ uncurry (joinPath y (Settings.staticRoot $ settings y)) $ renderRoute s
--     urlRenderOverride _ _ = Nothing

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent = addStaticContentExternal minifym base64md5 Settings.staticDir (StaticR . flip StaticRoute [])

    -- Place Javascript at bottom of the body tag so the rest of the page loads first
    jsLoader _ = BottomOfBody

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLog _ _source level =
        development || level == LevelWarn || level == LevelError

    -- Permits a query which can hold the request and the file if its the upload
    -- page.
    maximumContentLength app page =
        case page of
            Just UploadR -> maxFileSize + maxRequestSize
            _            -> maxRequestSize
      where
        extras = appExtra $ settings app
        maxFileSize = extraMaxFileSize extras
        maxRequestSize = extraMaxRequestSize extras

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlPersist
    runDB f = do
        master <- getYesod
        Database.Persist.Store.runPool
            (persistConfig master)
            f
            (connPool master)

-- instance YesodAuth App where
--     type AuthId App = UserId
-- 
--     -- Where to send a user after successful login
--     loginDest _ = HomeR
--     -- Where to send a user after logout
--     logoutDest _ = HomeR
-- 
--     getAuthId creds = runDB $ do
--         x <- getBy $ UniqueUser $ credsIdent creds
--         case x of
--             Just (Entity uid _) -> return $ Just uid
--             Nothing -> do
--                 fmap Just $ insert $ User (credsIdent creds) Nothing
-- 
--     -- You can add other plugins like BrowserID, email or OAuth here
--     authPlugins _ = [authBrowserId, authGoogleEmail]
-- 
--     authHttpManager = httpManager

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

-- | Get the 'Extra' value, used to hold data from the settings.yml file.
getExtra :: Handler Extra
getExtra = fmap (appExtra . settings) getYesod

-- Note: previous versions of the scaffolding included a deliver function to
-- send emails. Unfortunately, there are too many different options for us to
-- give a reasonable default. Instead, the information is available on the
-- wiki:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email

-- | Reads the session value to get the admin key of the visitor. Returns
-- 'Norhing' if the user doesn\'t have a key.
-- The admin key is a random key given to each user to control their own 
-- uploads.
tryAdminKey :: GHandler sub App (Maybe AdminKey)
tryAdminKey = do
    mKey <- lookupSession "admin_key"
    return $ (read . unpack) `fmap` mKey

-- | Reads the session value to get the admin key of the visitor. If the user
-- doesn\'t have a key, creates a new key.
getAdminKey :: GHandler sub App AdminKey
getAdminKey = do
    mKey <- tryAdminKey

    -- Checks if the user has already an admin key.
    case mKey of
        Just k ->
            return k
        Nothing -> runDB $ do
            -- The user hasn't admin key. Takes the next free admin key.
            mLastK <- selectFirst [] []
            k <- case mLastK of
                Just (Entity keyId (LastAdminKey lastK)) -> do
                    -- Increments the last admin key to get a new value.
                    update keyId [LastAdminKeyValue +=. 1]
                    return $ lastK + 1
                Nothing -> do
                    -- Inserts the first admin key in the database.
                    _ <- insert (LastAdminKey 0)
                    return 0

            lift $ setSession "admin_key" (pack $ show k)
            return k
