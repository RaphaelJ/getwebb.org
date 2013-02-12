{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE RankNTypes #-}
module Account.Foundation where

import Prelude
import Language.Haskell.TH (Pred (..), Type (..), mkName)
import Data.Text (Text)

import Yesod

data Account = Account {
      -- | The public and the private reCaptcha keys.
      acRecaptchaKeys :: (Text, Text)
    }

-- | Defines a few getters and lookup functions to interact with the master
-- site routes and entities.
class (Yesod master, RenderMessage master FormMessage) =>
      YesodAccount master where
    type AccountUser master

    -- | Post sign in/sign out routes.
    signInDest, signOutDest :: master -> Route master

    -- | Fetches the user from the database.
    emailLookup, usernameLookup ::
        Text -> GHandler sub master (Maybe (Entity (AccountUser master)))

    -- | Accesses data from the user account data type.
    accountEmail, accountUsername, accountPassword, accountSalt ::
        AccountUser master -> Text

mkYesodSubData "Account"
    [ ClassP ''YesodAccount [VarT $ mkName "master"] ]
    $(parseRoutesFile "config/routes_account")

type AccountHandler x = YesodAccount master => GHandler Account master x

type AccountForm x = YesodAccount master =>
    Html -> MForm Account master (FormResult x, GWidget Account master ())
