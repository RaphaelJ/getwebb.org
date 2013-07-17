{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Foundation where

import Prelude
import Control.Applicative ((<$>), (<*>), (<|>))
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Char8 as S8
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (Typeable)
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
import Text.Julius (rawJS)
import Text.Hamlet (hamletFile)
import qualified Web.ClientSession as S

import Account
import Handler.Download.ViewsCache.Type (ViewsCache)
import JobsDaemon.Type (JobsQueue)
import Model
import Util.Form (twitterField, checkLength)
import Util.Hmac.Type (Hmac)
import Util.Pretty (PrettyNumber (..), wrappedText)

-- | The site argument for the application.
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
    , viewsCache    :: ViewsCache
    }

mkYesodData "App" $(parseRoutesFile "config/routes")

type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

encryptKeyFile :: FilePath
encryptKeyFile = "config/client_session_key.aes"

-- | Used to change the state of the main navigation bar.
data CurrentPage = NewUpload | Browser | History | Other deriving (Eq)

-- | Used to change the state of the user navigation bar.
data CurrentUserPage = 
    UserProfile UserId | UserHistory | UserSettings | NotUserPage
    deriving (Eq)

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    approot = ApprootMaster $ appRoot . settings

    errorHandler e = do
        let ((title :: Text), mMsg, mMarkup) = case e of
                NotFound ->
                    let markup = [shamlet|
                            The page you are looking for is no longer available.
                            <br />
                            If this was an uploaded file, it has been removed.
                        |]
                    in ("404 Not Found", Nothing, Just markup)
                InternalError err ->
                    let markup = [shamlet|
                            Our internal software failed for the following
                            reason :
                            <pre>
                                #{err}
                        |]
                    in ("500 Internal Server Error", Just err, Just markup)
                InvalidArgs _    -> ("400 Invalid Arguments", Nothing, Nothing)
                NotAuthenticated -> ("401 Not Authenticated", Nothing, Nothing)
                PermissionDenied err ->
                    let markup = [shamlet|
                            You don't have the permission to access this
                            ressource for the following reason :
                            <pre>
                                #{err}
                        |]
                    in ("403 Permission Denied", Just err, Just markup)
                BadMethod m ->
                    let markup = [shamlet|
                            Method <code>#{S8.unpack m}</code> not supported
                        |]
                    in ("405 Method Not Allowed", Nothing, Just markup)

        selectRep $ do
            provideRep $ defaultLayout $ do
                setTitle [shamlet|#{title} - getwebb|]
                $(widgetFile "error")

            provideJson $
                case mMsg of
                    Just msg -> array [
                            renderMarkup [shamlet| #{title} : #{msg}|]
                        ]
                    Nothing  -> array [title]

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is two year.
    makeSessionBackend _ = do
        key <- S.getKey encryptKeyFile
        let timeout = 2 * 365 * 24 * 3600 -- 2 years
        (getCachedDate, _closeDateCache) <- clientSessionDateCacher timeout
        return . Just $ clientSessionBackend key getCachedDate

    defaultLayout widget = do
        app <- getYesod
        extra <- getExtra
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

            -- Google analytics
            toWidget [julius|
                var _gaq = _gaq || [];
                _gaq.push(['_setAccount', '#{rawJS $ extraAnalytics extra}']);
                _gaq.push(['_trackPageview']);

                (function() {
                    var ga = document.createElement('script');
                    ga.type = 'text/javascript'; ga.async = true;
                    ga.src = ('https:' == document.location.protocol ? 'https://ssl' : 'http://www') + '.google-analytics.com/ga.js';
                    var s = document.getElementsByTagName('script')[0];
                    s.parentNode.insertBefore(ga, s);
                })();
            |]

        giveUrlRenderer $(hamletFile "templates/default-layout.hamlet")
      where
        getCurrentPage (Just UploadR)  = NewUpload
        getCurrentPage (Just BrowserR) = Browser
        getCurrentPage (Just HistoryR) = History
        getCurrentPage _               = Other

--     urlRenderOverride app (StaticR s) =
    urlRenderOverride app route =
        case route of AccountR _ -> overrideRoot extraAccountRoot
                      StaticR  _ -> overrideRoot extraStaticRoot
                      _          -> Nothing
      where
        overrideRoot newRoot =
            case newRoot extras of
                Just root ->
                    let (path, queries) = renderRoute route
                    in Just $ joinPath app root path queries
                Nothing   -> Nothing

        extras = appExtra $ settings app

    authRoute _ = Just $ AccountR SignInR

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent = addStaticContentExternal minifym base64md5
                                                Settings.staticDir
                                                (StaticR . flip StaticRoute [])

    -- Place Javascript at bottom of the body tag so the rest of the page loads 
    -- first
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

data UserAccountSettings = UserAccountSettings {
      usBio           :: Maybe Textarea
    , usLocation      :: Maybe Text
    , usWebsite       :: Maybe Text
    , usTwitter       :: Maybe Text
    , usDefaultPublic :: Bool
    }

instance YesodAccount App where
    type AccountUser     App = User
    type AccountSettings App = UserAccountSettings

    usesReverseProxy = extraReverseProxy . appExtra . settings

    signInDest  _ = HistoryR
    signOutDest _ = HistoryR

    initUser email name pass salt hostname created avatar = do
        adminKey <- newAdminKey
        return User {
              userEmail = email, userName = name, userPassword = pass
            , userSalt = salt, userHostname = hostname, userCreated = created
            , userAvatar = avatar, userAdmin = False, userAdminKey = adminKey
            , userCommentsCount = 0, userBio = Nothing, userLocation = Nothing
            , userWebsite = Nothing, userTwitter = Nothing
            , userDefaultPublic = True
            }

    emailLookup    _ = UniqueUserEmail
    usernameLookup _ = UniqueUserName
    hostnameLookup _ = UserHostname
    createdLookup  _ = UserCreated

    accountEmail    _ = userEmail
    accountUsername _ = userName
    accountPassword _ = userPassword
    accountSalt     _ = userSalt
    accountHostname _ = userHostname
    accountAvatarId _ = userAvatar

    accountAvatarIdField _ = UserAvatar

    accountSettingsWidget app user avatar = do
        let mUser           = Just (user, avatar)
            currentUserPage = UserSettings
        $(widgetFile "modules/user-bar")

    accountSettingsForm user = UserAccountSettings
        <$> aopt (checkLength maxUserBioLength textareaField) bioSettings
                 (Just (userBio user))
        <*> aopt (checkLength maxUserLocationLength textField) "Location"
                 (Just (userLocation user))
        <*> aopt (checkLength maxUserWebsiteLength urlField) "Website"
                 (Just (userWebsite user))
        <*> aopt twitterField twitterSettings (Just (userTwitter user))
        <*> areq checkBoxField "Set uploads as public by default"
                 (Just (userDefaultPublic user))
      where
        bioSettings = FieldSettings {
              fsLabel = "Say something about yourself"
            , fsTooltip = Just "or let empty."
            , fsId = Nothing, fsName = Nothing, fsAttrs = []
        }

        twitterSettings = FieldSettings {
              fsLabel = "Twitter"
            , fsTooltip = Just "Your twitter account without the starting @."
            , fsId = Nothing, fsName = Nothing, fsAttrs = []
        }

    accountSettingsSave userId setts =
        update userId [ UserBio           =. usBio            setts
                      , UserLocation      =. usLocation       setts
                      , UserWebsite       =. usWebsite        setts
                      , UserTwitter       =. usTwitter        setts
                      , UserDefaultPublic =. usDefaultPublic  setts ]

    avatarsDir _ = Settings.staticDir </> "avatars"
    avatarsDirRoute _ path = StaticR $ StaticRoute ("avatars" : path) []

-- Admin Keys ------------------------------------------------------------------

-- | Each upload is associated to an 'AdminKey'. Anonymous got an 'AdminKey' in
-- their cookies whereas authenticated users have an associated 'AdminKey'.
-- An 'AdminKey' gives some privileges to the corresponding uploads to its
-- owner.
data AdminKeyValue =
    -- | Authenticated users can have up to two 'AdminKey's and can be
    -- administrators (last field equals 'True').
      AdminKeyUser (Entity AdminKey) (Maybe (Entity AdminKey)) Bool
    | AdminKeyAnon (Entity AdminKey)
    deriving (Typeable)

adminKeySessionKey :: Text
adminKeySessionKey = "ADMIN_KEY"

-- | Allocates a new 'AdminKey' in the database. Doesn't set the session key.
newAdminKey :: YesodDB App AdminKeyId
newAdminKey = insert $ AdminKey 0

-- | Sets the 'AdminKey' in the user's session cookie.
setAdminKey :: AdminKeyId -> Handler ()
setAdminKey = setSession adminKeySessionKey . T.pack . show

-- | Returns the admin key(s). Checks that it exists in the database or returns
-- 'Nothing' if the client doesn\'t have an 'AdminKey' yet.
getAdminKey :: Handler (Maybe AdminKeyValue)
getAdminKey = cached $ runMaybeT $
        do
        (Entity _ user, _) <- MaybeT getUser
        MaybeT $ runDB $ runMaybeT $ do
            let keyId = userAdminKey user
            key        <- MaybeT $ get keyId
            mCookieKey <- lift getCookieKey
            return $ AdminKeyUser (Entity keyId key) mCookieKey (userAdmin user)
    <|> (AdminKeyAnon <$> MaybeT (runDB getCookieKey))
  where
    -- Retrieves the AdminKey in the browser's session from the database, if
    -- any.
    getCookieKey = runMaybeT $ do
        keyId' <- MaybeT $ lift $ lookupSession adminKeySessionKey
        let keyId = read $ T.unpack keyId'
        key    <- MaybeT $ get keyId
        return $ Entity keyId key

-- | Returns the total number of upload of the user.
uploadsCount :: Maybe AdminKeyValue -> Int
uploadsCount k =
    case k of Just (AdminKeyUser a Nothing  _) -> keyCount a
              Just (AdminKeyUser a (Just b) _) -> keyCount a + keyCount b
              Just (AdminKeyAnon a)            -> keyCount a
              Nothing                          -> 0
  where
    keyCount = adminKeyCount . entityVal

-- | Returns True if the client has administrator rights on the upload.
isAdmin :: Upload -> Maybe AdminKeyValue -> Bool
isAdmin upload key =
    case key of Just (AdminKeyUser _ _        True)             -> True
                Just (AdminKeyUser a _        _)    | sameKey a -> True
                Just (AdminKeyUser _ (Just b) _)    | sameKey b -> True
                Just (AdminKeyAnon a)               | sameKey a -> True
                _                                               -> False
  where
    sameKey = (uploadAdminKey upload ==) . entityKey

-- Extras ----------------------------------------------------------------------

-- | Get the 'Extra' value, used to hold data from the settings.yml file.
getExtra :: Handler Extra
getExtra = fmap (appExtra . settings) getYesod
