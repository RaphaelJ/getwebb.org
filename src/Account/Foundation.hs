{-# LANGUAGE ImpredicativeTypes, RankNTypes #-}
module Account.Foundation where

import Prelude
import Data.Int
import qualified Data.Array as A
import Data.Text (Text)

import Database.Persist.Sql (SqlBackend)
import Yesod

import qualified Vision.Image as I

-- | The sprite contains the set of patterns which can be used to build a random
-- avatar (identicon).
newtype Sprite = Sprite (A.Array Int I.GreyImage)

data Account = Account {
      -- | The public and the private reCaptcha keys.
      acRecaptchaKeys :: (Text, Text)
    , acAvatarSprite :: Sprite
    }

-- | The key used in the sessions to store the user's ID.
sessionKey :: Text
sessionKey = "_ACCOUNT_ID"

share [mkPersist sqlOnlySettings, mkMigrate "migrateAccount"] [persistLowerCase|
-- Manages how many users share a same avatar.
AvatarFile
    hash Text -- SHA1 of the resized file.
    count Int -- Number of users sharing this avatar.
    UniqueAvatarFileHash hash
    deriving Show

Avatar
    generated Bool
    hash Text
    deriving Show
|]

type AvatarNum = Int64

type AccountHandler parent = HandlerT Account (HandlerT parent IO)
type ParentHandler  parent = HandlerT parent  IO
type ParentWidget   parent = WidgetT  parent  IO

-- | Defines a few parameters, getters and lookup functions to interact with the
-- parent site routes and entities.
class (Yesod parent, YesodPersist parent, RenderMessage parent FormMessage
      , PersistEntity (AccountUser parent)
      , PersistEntityBackend (AccountUser parent)
        ~ PersistMonadBackend (YesodDB parent)
      , PersistEntityBackend Avatar ~ PersistMonadBackend (YesodDB parent)
      , PersistEntityBackend (AccountUser parent) ~ SqlBackend
      , PersistMonadBackend (YesodDB parent) ~ SqlBackend
      , PersistQuery (YesodDB parent)
      , PersistUnique (YesodDB parent)
      , HandlerSite (YesodDB parent) ~ parent
      , MonadHandler (YesodDB parent)
      , MonadTrans (YesodPersistBackend parent)
      , Functor (YesodDB parent)) => YesodAccount parent where
    type AccountUser     parent :: *
    type AccountSettings parent :: *

    -- | Post sign in/sign out routes.
    signInDest, signOutDest :: parent -> Route parent

    -- | Initialise a new user value (musn't add the user to the database).
    initUser :: Text      -- ^ Email
             -> Text      -- ^ Username
             -> Text      -- ^ Hashed password
             -> Text      -- ^ Salt used to hash the password
             -> AvatarNum
             -> YesodDB parent (AccountUser parent)

    -- | Unique keys to fetch users from the database.
    emailLookup, usernameLookup :: parent -> Text -> Unique (AccountUser parent)

    -- | Accesses data from the user account data type.
    accountEmail, accountUsername, accountPassword, accountSalt ::
        parent -> AccountUser parent -> Text

    accountAvatarId :: parent -> AccountUser parent -> AvatarNum

    -- | Used to change the avatar\' ID of the user.
    accountAvatarIdField :: parent -> EntityField (AccountUser parent) AvatarNum

    -- | Form used on the settings page and which is added to the avatar form.
    accountSettingsForm :: (MonadHandler m, HandlerSite m ~ parent) =>
                           AccountUser parent
                        -> AForm m (AccountSettings parent)

    -- | Saves the new user settings to the database.
    accountSettingsSave :: Key (AccountUser parent) -> (AccountSettings parent)
                        -> YesodDB parent ()

    -- | Directory where avatars will be stored.
    avatarsDir :: parent -> FilePath

   -- | How to get a route in the avatars directory.
    avatarsDirRoute :: parent -> [Text] -> Route parent

mkYesodSubData "Account"
    $(parseRoutesFile "config/routes_account")
