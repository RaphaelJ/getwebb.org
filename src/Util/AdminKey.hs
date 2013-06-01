{-# LANGUAGE OverloadedStrings #-}
-- | Defines functions to allocate and 
module Util.AdminKey (
      AdminKey (..), AdminKeyId {- from Model -}
    , newAdminKey, tryAdminKey, getAdminKey
    ) where

import Model

-- | Allocates a new 'AdminKey' in the database, linking it to a potential user.
newAdminKey :: YesodDB sub App AdminKeyId
newAdminKey = insert . AdminKey 0

-- | Reads the session value to get the admin key of the visitor. Returns
-- 'Nothing' if the user doesn\'t have a key.
-- The admin key is a random key given to each user to control their own 
-- uploads.
tryAdminKey :: GHandler sub App (Maybe AdminKeyId)
tryAdminKey = do
    mKey <- lookupSession "ADMIN_KEY"
    return $ (read . unpack) `fmap` mKey

-- | Reads the session value to get the admin key of the visitor. If the user
-- doesn\'t have a key, creates a new key.
getAdminKey :: GHandler sub App AdminKeyId
getAdminKey = do
    mKey <- tryAdminKey

    -- Checks if the user has already an admin key.
    case mKey of
        Just k ->
            return k
        Nothing -> do
            k <- runDB $ newAdminKey Nothing

            setSession "ADMIN_KEY" (pack $ show k)
            return k
