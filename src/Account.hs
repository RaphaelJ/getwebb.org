{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
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

import Account.Auth as Account

mkYesodSubDispatch "Account" 
    [ ClassP ''YesodAccount [VarT $ mkName "master"] ]
    resourcesAccount
