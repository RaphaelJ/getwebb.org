{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE RankNTypes #-}
module Account.Foundation where

import Prelude
import Control.Monad.Trans (MonadTrans)
import Data.Int
import qualified Data.Array as A
import Data.Text (Text)
import Language.Haskell.TH (Pred (..), Type (..), mkName)

import Yesod
import qualified Vision.Image as I

data Account = Account {
      -- | The public and the private reCaptcha keys.
      acRecaptchaKeys :: (Text, Text)
    , acAvatarSprite :: A.Array Int I.GreyImage
    }

-- | The key used in the sessions to store the user's ID.
sessionKey :: Text
sessionKey = "_ACCOUNT_ID"

share [mkPersist sqlOnlySettings, mkMigrate "migrateAccount"] [persistLowerCase|
-- Manage how many users share a same avatar.
Avatar
    hash Text -- SHA1 of the resized file.
    generated Bool
    count Int -- Number of users using this avatar.
    UniqueAvatarHash hash
    deriving Show
|]

-- | Defines a few parameters, getters and lookup functions to interact with the 
-- master site routes and entities.
class (Yesod master, YesodPersist master, RenderMessage master FormMessage
      , PersistEntity (AccountUser master)
      , PersistEntityBackend (AccountUser master)
        ~ PersistMonadBackend (YesodDB Account master)
      , PersistEntityBackend Avatar
        ~ PersistMonadBackend (YesodDB Account master)
      , PersistQuery (YesodDB Account master)
      , PersistUnique (YesodDB Account master)
      , MonadTrans (YesodPersistBackend master)
      , Functor (YesodDB Account master)) => YesodAccount master where
    type AccountUser     master :: *
    type AccountSettings master :: *

    -- | Post sign in/sign out routes.
    signInDest, signOutDest :: master -> Route master

    -- | Initialise a new user value (musn't add the user to the database).
    initUser :: Text     -- ^ Email
             -> Text     -- ^ Username
             -> Text     -- ^ Salted password
             -> Text     -- ^ Salt
             -> Int64    -- ^ Avatar\'s ID
             -> GHandler sub master (AccountUser master)

    -- | Unique keys to fetch users from the database.
    emailLookup, usernameLookup :: master -> Text -> Unique (AccountUser master)

    -- | Accesses data from the user account data type.
    accountEmail, accountUsername, accountPassword, accountSalt ::
        master -> AccountUser master -> Text

    accountAvatar :: master -> AccountUser master -> Int64

    -- | Form used on the settings page and which is added to the avatar form.
    accountSettingsForm :: AccountUser master
                        -> AForm sub master (AccountSettings master)

    -- | Directory where avatars will be stored.
    avatarsDir :: master -> FilePath

   -- | How to get a route in the avatars directory.
    avatarsDirRoute :: master -> [Text] -> Route master

mkYesodSubData "Account"
    [ ClassP ''YesodAccount [VarT $ mkName "master"] ]
    $(parseRoutesFile "config/routes_account")
