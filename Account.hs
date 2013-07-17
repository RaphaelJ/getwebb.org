{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Provides a sub-site which manages registration, authentication and
-- settings of user accounts.
module Account (
      module Account
    )
    where

import Prelude

import Yesod
import Account.Foundation as Account
import Account.Util as Account
import Account.Avatar as Account

import Account.Auth
import Account.Register
import Account.Settings

instance YesodAccount master =>
         YesodSubDispatch Account (HandlerT master IO) where
    yesodSubDispatch = $(mkYesodSubDispatch resourcesAccount)

-- | Initializes a new 'Account' foundation type.
makeAccount :: IO Account
makeAccount = do
    sprite <- loadSprite
    return $! Account sprite
