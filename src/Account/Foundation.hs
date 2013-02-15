{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE RankNTypes #-}
module Account.Foundation where

import Prelude
import Data.Text (Text)
import Language.Haskell.TH (Pred (..), Type (..), mkName)

import Yesod

data Account = Account {
      -- | The public and the private reCaptcha keys.
      acRecaptchaKeys :: (Text, Text)
    }

-- | The key used in the sessions to store the user's ID.
sessionKey :: Text
sessionKey = "_ACCOUNT_ID"

-- | Defines a few parameters, getters and lookup functions to interact with the 
-- master site routes and entities.
class (Yesod master, YesodPersist master, RenderMessage master FormMessage
      , PersistEntityBackend (AccountUser master)
        ~ PersistMonadBackend (YesodDB Account master)
      , PersistUnique (YesodDB Account master)
      , PersistEntity (AccountUser master)) => YesodAccount master where
    type AccountUser master

    -- | Post sign in/sign out routes.
    signInDest, signOutDest :: master -> Route master

    -- | Unique keys to fetch users from the database.
    emailLookup, usernameLookup :: Text -> Unique (AccountUser master)

    -- | Accesses data from the user account data type.
    accountEmail, accountUsername, accountPassword, accountSalt ::
        AccountUser master -> Text

mkYesodSubData "Account"
    [ ClassP ''YesodAccount [VarT $ mkName "master"] ]
    $(parseRoutesFile "config/routes_account")

type AccountHandler x = YesodAccount master => GHandler Account master x

type AccountForm x = YesodAccount master =>
    Html -> MForm Account master (FormResult x, GWidget Account master ())
