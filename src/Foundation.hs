{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Foundation where

import Prelude
import Control.Concurrent (Chan, MVar)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Char8 as S8
import qualified Data.Map as M
import Data.Text (Text, pack, unpack)
import Data.Time.Clock (UTCTime)
import Data.Word

import Yesod
import Yesod.Static
import Yesod.Default.Config
import Yesod.Default.Util (addStaticContentExternal)
import qualified Database.Persist.Store
import Database.Persist.GenericSql
import Network.HTTP.Conduit (Manager)
import qualified Settings
import Settings (widgetFile, Extra (..))
import Settings.Development (development)
import Settings.StaticFiles as Import
import Text.Blaze.Renderer.Text (renderMarkup)
import Text.Jasmine (minifym)
import Text.Hamlet (shamlet, hamletFile)
import qualified Web.ClientSession as S

import Account.Foundation
import Model
import Util.Pretty (PrettyNumber (..))

-- | Contains a queue of background jobs which can be processed right now.
type JobsChan = Chan (JobId, IO ())
-- | Maps each uncompleted parent job to a list of its dependent jobs.
-- Each mapped job contains its id, its action and possibly the list of the
-- remaining other dependencies of the job.
-- When a job completes, the corresponding entry from the map is removed and
-- each mapped job is :
--      - added to the 'JobsChan' if the list of remaining dependencies of the
--      job is empty ;
--      - added to the first job of its remaining dependencies if the list is
--      not empty.
type JobsDepends = MVar (M.Map JobId [(JobId, IO (), [JobId])])
-- | Contains the queue of jobs which can be processed and the graph of
-- dependencies.
type JobsQueue = (JobsChan, JobsDepends)

-- | The site argument for your application. This can be a good place to
-- keep settings and values requiring initialisation before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App {
      settings :: AppConfig DefaultEnv Extra
    , getStatic :: Static -- ^ Settings for static file serving.
    , getAccount :: Account -- ^ Account management subsite.
    , connPool :: Database.Persist.Store.PersistConfigPool Settings.PersistConfig -- ^ Database connection pool.
    , httpManager :: Manager
    , persistConfig :: Settings.PersistConfig
    , encryptKey :: B.ByteString
    , jobsQueue :: JobsQueue
    , viewsBuffer :: (MVar (M.Map UploadId (Word64, Maybe UTCTime, Word64)), MVar ())
    }

-- Set up i18n messages. See the message folder.
-- mkMessage "App" "messages" "en"

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

encryptKeyFile :: FilePath
encryptKeyFile = "config/client_session_key.aes"

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    approot = ApprootMaster $ appRoot . settings

    errorHandler e = do
        let ((title :: Text), mMsg) = case e of
                NotFound ->
                    let msg = [shamlet|
                            The page you are looking for is no longer available.
                            <br />
                            If this was an uploaded file, it has been removed.
                        |]
                    in ("404 Not Found", Just msg)
                PermissionDenied err ->
                    let msg = [shamlet|
                            You don't have the permission to access this
                            ressource for the following reason :
                            <pre>
                                #{err}
                        |]
                    in ("403 Permission Denied", Just msg)
                InvalidArgs _ -> ("Invalid Arguments", Nothing)
                InternalError err ->
                    let msg = [shamlet|
                            Our internal software failed for the following
                            reason :
                            <pre>
                                #{err}
                        |]
                    in ("500 Internal Server Error", Just msg)
                BadMethod m ->
                    let msg = [shamlet|
                            Method <code>#{S8.unpack m}</code> not supported
                        |]
                    in ("405 Method Not Allowed", Just msg)

        let repHtml = do
                setTitle [shamlet|#{title} - getwebb|]
                $(widgetFile "error")
            repJson = case mMsg of
                Just msg -> object [(title, renderMarkup msg)]
                Nothing  -> object [(title, "" :: Text)]
        rep <- defaultLayoutJson repHtml repJson
        return $ chooseRep rep

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is two year.
    makeSessionBackend _ = do
        key <- S.getKey encryptKeyFile
        let timeout = 2 * 365 * 24 * 3600 -- 2 years
        (getCachedDate, _closeDateCache) <- clientSessionDateCacher timeout
        return . Just $ clientSessionBackend2 key getCachedDate

    defaultLayout widget = do
        app <- getYesod
        mMsg <- getMessage

        toMaster <- getRouteToMaster
        currentRoute <- getCurrentRoute >>= return . fmap toMaster

        mAdminKeyId <- tryAdminKey
        countHistory <- case mAdminKeyId of
            Just adminKeyId -> do
                Just adminKey <- runDB $ get adminKeyId
                return $! adminKeyCount adminKey
            Nothing -> return 0

        pc <- widgetToPageContent $ do
            $(widgetFile "normalize")
            $(widgetFile "default")
            $(widgetFile "default-header")
            $(widgetFile "default-body")
            $(widgetFile "default-footer")
            addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.9.0/jquery.min.js"
        hamletToRepHtml $(hamletFile "templates/default-layout.hamlet")

    -- This is done to provide an optimization for serving static files from
    -- a separate domain. Please see the staticRoot setting in Settings.hs
--     urlRenderOverride y (StaticR s) =
--         Just $ uncurry (joinPath y (Settings.staticRoot $ settings y)) $ renderRoute s
--     urlRenderOverride _ _ = Nothing

    authRoute _ = Just $ AccountR AuthR

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

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

instance YesodAccount App where
    type AccountUser App = User

    signInDest _  = HistoryR
    signOutDest _ = HistoryR

    initUser email name pass salt = return User {
          userEmail = email, userName = name, userPassword = pass
        , userSalt = salt, userAvatar = False, userIsAdmin = False
        , userPublic = True
        }

    emailLookup    = return . UniqueUserEmail
    usernameLookup = return . UniqueUserName

    accountEmail    = return . userEmail
    accountUsername = return . userName
    accountPassword = return . userPassword
    accountSalt     = return . userSalt

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
-- 'Nothing' if the user doesn\'t have a key.
-- The admin key is a random key given to each user to control their own 
-- uploads.
tryAdminKey :: GHandler sub App (Maybe AdminKeyId)
tryAdminKey = do
    mKey <- lookupSession "ADMIN_KEY"
    return $ (read . unpack) `fmap` mKey

-- | Reads the         session value to get the admin key of the visitor. If the user
-- doesn\'t have a key, creates a new key.
getAdminKey :: GHandler sub App AdminKeyId
getAdminKey = do
    mKey <- tryAdminKey

    -- Checks if the user has already an admin key.
    case mKey of
        Just k ->
            return k
        Nothing -> do
            k <- runDB $ insert (AdminKey 0 Nothing)

            setSession "ADMIN_KEY" (pack $ show k)
            return k
