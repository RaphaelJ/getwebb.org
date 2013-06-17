{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Foundation where

import Prelude
import Control.Applicative ((<$>), (<|>))
import Control.Concurrent (MVar)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Char8 as S8
import qualified Data.Map as M
import Data.Text (Text, pack, unpack)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Typeable (Typeable)
import Data.Word
import System.FilePath ((</>))

import Yesod
import Yesod.Static
import Yesod.Default.Config
import Yesod.Default.Util (addStaticContentExternal)
import Database.Persist.Sql (SqlPersistT)
import Network.HTTP.Conduit (Manager)
import qualified Settings
import Settings (widgetFile, Extra (..))
import Settings.Development (development)
import Settings.StaticFiles as Import
import System.Log.FastLogger (Logger)
import Text.Blaze.Renderer.Text (renderMarkup)
import Text.Jasmine (minifym)
import Text.Hamlet (shamlet, hamletFile)
import qualified Web.ClientSession as S

import Account
import JobsDaemon.Type (JobsQueue)
import Model
import Util.Hmac.Type (Hmac)
import Util.Pretty (PrettyNumber (..))

-- | The site argument for your application. This can be a good place to
-- keep settings and values requiring initialisation before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App {
      settings      :: AppConfig DefaultEnv Extra
    , getStatic     :: Static       -- ^ Settings for static file serving.
    , getAccount    :: Account      -- ^ Account management subsite.
    , connPool      :: PersistConfigPool Settings.PersistConf
    , httpManager   :: Manager
    , persistConfig :: Settings.PersistConf
    , appLogger     :: Logger
    , encryptKey    :: B.ByteString
    , jobsQueue     :: JobsQueue
    , viewsBuffer   :: (MVar (M.Map UploadId (Word64, Maybe UTCTime, Word64))
                       , MVar ())   -- ^ Used by Handler.Download.
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

type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

encryptKeyFile :: FilePath
encryptKeyFile = "config/client_session_key.aes"

-- | Used to change the state of the navigation bar.
data CurrentPage = NewUpload | History | Other deriving (Eq)

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
                InternalError err ->
                    let msg = [shamlet|
                            Our internal software failed for the following
                            reason :
                            <pre>
                                #{err}
                        |]
                    in ("500 Internal Server Error", Just msg)
                InvalidArgs _ -> ("Invalid Arguments", Nothing)
                NotAuthenticated -> ("401 Not Authenticated", Nothing)
                PermissionDenied err ->
                    let msg = [shamlet|
                            You don't have the permission to access this
                            ressource for the following reason :
                            <pre>
                                #{err}
                        |]
                    in ("403 Permission Denied", Just msg)
                BadMethod m ->
                    let msg = [shamlet|
                            Method <code>#{S8.unpack m}</code> not supported
                        |]
                    in ("405 Method Not Allowed", Just msg)

        selectRep $ do
            provideRep $ defaultLayout $ do
                setTitle [shamlet|#{title} - getwebb|]
                $(widgetFile "error")

            provideJson $ object [title .= maybe "" renderMarkup mMsg]

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is two year.
    makeSessionBackend _ = do
        key <- S.getKey encryptKeyFile
        let timeout = 2 * 365 * 24 * 3600 -- 2 years
        (getCachedDate, _closeDateCache) <- clientSessionDateCacher timeout
        return . Just $ clientSessionBackend key getCachedDate

    defaultLayout widget = do
        app <- getYesod
        mMsg <- getMessage

        currentPage <- getCurrentPage <$> getCurrentRoute

        countHistory <- uploadsCount <$> getAdminKey

        pc <- widgetToPageContent $ do
            $(widgetFile "normalize")
            $(widgetFile "default")
            $(widgetFile "default-header")
            $(widgetFile "default-body")
            $(widgetFile "default-footer")
            addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.9.0/jquery.min.js"
        hamletToRepHtml $(hamletFile "templates/default-layout.hamlet")
      where
        getCurrentPage (Just UploadR)  = NewUpload
        getCurrentPage (Just HistoryR) = History
        getCurrentPage _               = Other

    -- This is done to provide an optimization for serving static files from
    -- a separate domain. Please see the staticRoot setting in Settings.hs
--     urlRenderOverride y (StaticR s) =
--         Just $ uncurry (joinPath y (Settings.staticRoot $ settings y)) $ renderRoute s
--     urlRenderOverride _ _ = Nothing

    authRoute _ = Just $ AccountR SignInR

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent = addStaticContentExternal minifym base64md5
                                                Settings.staticDir
                                                (StaticR . flip StaticRoute [])

    -- Place Javascript at bottom of the body tag so the rest of the page loads first
    jsLoader _ = BottomOfBody

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLog _ _source level =
        development || level == LevelWarn || level == LevelError

    -- Permits a query which can hold the request and the file if its the upload
    -- page.
    maximumContentLength app page =
        Just $ case page of
            Just UploadR -> maxFileSize + maxRequestSize
            _            -> maxRequestSize
      where
        extras = appExtra $ settings app
        maxFileSize = extraMaxFileSize extras
        maxRequestSize = extraMaxRequestSize extras

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlPersistT
    runDB f = do
        master <- getYesod
        runPool (persistConfig master) f (connPool master)

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

-- Accounts --------------------------------------------------------------------

data UserSettings = UserSettings { usDefaultPublic :: Bool }

instance YesodAccount App where
    type AccountUser     App = User
    type AccountSettings App = UserSettings

    signInDest  _ = HistoryR
    signOutDest _ = HistoryR

    initUser email name pass salt avatar = do
        time <- liftIO $ getCurrentTime
        adminKey <- newAdminKey
        return User {
              userEmail = email, userName = name, userPassword = pass
            , userSalt = salt, userCreated = time, userAvatar = avatar
            , userIsAdmin = False, userAdminKey = adminKey
            , userCommentsCount = 0, userDefaultPublic = True
            }

    emailLookup    _ = UniqueUserEmail
    usernameLookup _ = UniqueUserName

    accountEmail    _ = userEmail
    accountUsername _ = userName
    accountPassword _ = userPassword
    accountSalt     _ = userSalt
    accountAvatarId _ = userAvatar

    accountAvatarIdField _ = UserAvatar

    accountSettingsForm user =
        UserSettings <$> areq checkBoxField
                              "Set uploads as public by default"
                              (Just (userDefaultPublic user))

    accountSettingsSave userId (UserSettings defaultPublic) =
        update userId [UserDefaultPublic =. defaultPublic]

    avatarsDir _ = Settings.staticDir </> "avatars"
    avatarsDirRoute _ path = StaticR $ StaticRoute ("avatars" : path) []

-- Admin Keys ------------------------------------------------------------------

-- | Each upload is associated to an 'AdminKey'. Anonymous got an 'AdminKey' in
-- their cookies whereas authenticated users have an associated 'AdminKey'.
-- An 'AdminKey' gives some privileges to the corresponding uploads to its
-- owner.
data AdminKeyValue = AdminKeyUser (Entity AdminKey) (Maybe (Entity AdminKey))
                   | AdminKeyAnon (Entity AdminKey)
                   deriving (Typeable)

adminKeySessionKey :: Text
adminKeySessionKey = "ADMIN_KEY"

-- | Allocates a new 'AdminKey' in the database. Doesn't set the session key.
newAdminKey :: YesodDB App AdminKeyId
newAdminKey = insert $ AdminKey 0

-- | Sets the 'AdminKey' in the user's session cookie.
setAdminKey :: AdminKeyId -> Handler ()
setAdminKey = setSession adminKeySessionKey . pack . show

-- | Returns the admin key(s). Checks that it exists in the database or returns
-- 'Nothing' if the client doesn\'t have an 'AdminKey' yet.
getAdminKey :: Handler (Maybe AdminKeyValue)
getAdminKey = cached $ runMaybeT $
        do
        (Entity _ user, _) <- MaybeT getUser
        MaybeT $ runDB $ runMaybeT $ do
            let keyId = userAdminKey user
            key <- MaybeT $ get keyId
            lift $ AdminKeyUser (Entity keyId key) <$> getCookieKey
    <|> (AdminKeyAnon <$> MaybeT (runDB getCookieKey))
  where
    -- Retrieves the AdminKey in the browser's session from the database, if
    -- any.
    getCookieKey = runMaybeT $ do
        keyId' <- MaybeT $ lift $ lookupSession adminKeySessionKey
        let keyId = read $ unpack keyId'
        key    <- MaybeT $ get keyId
        return $ Entity keyId key

-- | Returns the total number of upload of the user.
uploadsCount :: Maybe AdminKeyValue -> Int
uploadsCount k =
    case k of Just (AdminKeyUser a Nothing)  -> keyCount a
              Just (AdminKeyUser a (Just b)) -> keyCount a + keyCount b
              Just (AdminKeyAnon a)          -> keyCount a
              Nothing                        -> 0
  where
    keyCount = adminKeyCount . entityVal

-- | Returns True if the client is the administrator of the upload.
isAdmin :: Upload -> Maybe AdminKeyValue -> Bool
isAdmin upload key =
    case key of Just (AdminKeyUser a _)        | sameKey a -> True
                Just (AdminKeyUser _ (Just b)) | sameKey b -> True
                Just (AdminKeyAnon a)          | sameKey a -> True
                _                                          -> False
  where
    sameKey = (uploadAdminKey upload ==) . entityKey

-- Extras ----------------------------------------------------------------------

-- | Get the 'Extra' value, used to hold data from the settings.yml file.
getExtra :: Handler Extra
getExtra = fmap (appExtra . settings) getYesod

-- Note: previous versions of the scaffolding included a deliver function to
-- send emails. Unfortunately, there are too many different options for us to
-- give a reasonable default. Instead, the information is available on the
-- wiki:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
