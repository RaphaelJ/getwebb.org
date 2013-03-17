{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Provides a sub-site which manages registration, authentication and
-- user accounts.
module Account (
      module Account
    )
    where

import Prelude
import Language.Haskell.TH (Pred (..), Type (..), mkName)

import Yesod
import Account.Foundation as Account
import Account.Util as Account

import Account.Avatar as Account
import Account.Auth as Account
import Account.Settings as Account

mkYesodSubDispatch "Account"
    [ ClassP ''YesodAccount [VarT $ mkName "master"] ]
    resourcesAccount

-- | Initialize a new 'Account' foundation type.
makeAccount :: IO Account
makeAccount = do
    reCaptachaKeys <- read `fmap` readFile "config/recaptcha"
    sprite <- loadSprite
    return $! Account reCaptachaKeys sprite