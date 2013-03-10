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
import Database.Persist.GenericSql (SqlPersist, runMigration)
import Database.Persist.Store (
      PersistConfig, PersistConfigBackend, PersistConfigPool, runPool
    )

import Account.Foundation as Account
import Account.Util as Account

import Account.Avatar as Account
import Account.Auth as Account
import Account.Settings as Account

mkYesodSubDispatch "Account" 
    [ ClassP ''YesodAccount [VarT $ mkName "master"] ]
    resourcesAccount

-- | Initialize a new 'Account' foundation type.
makeAccount :: (PersistConfig c, PersistConfigBackend c ~ SqlPersist) =>
               c -> PersistConfigPool c -> IO Account
makeAccount conf pool = do
    reCaptachaKeys <- read `fmap` readFile "config/recaptcha"
    sprite <- loadSprite
    runPool conf (runMigration migrateAvatar) pool
    return $! Account reCaptachaKeys sprite
