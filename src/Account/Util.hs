{-# LANGUAGE BangPatterns #-}
-- | Provides functions to create and authenticate users.
module Account.Util (
      newUser, validateUser, setUserId, getUserId, getUser, unsetUserId
    , requireAuth, redirectAuth, redirectNoAuth
    , randomSalt, saltedHash
    ) where

import Prelude

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Digest.Pure.SHA (sha1, showDigest)
import Data.Text (Text)
import qualified Data.Text as T
import System.Random (randomRIO)

import Yesod

import Account.Avatar (genIdenticon, newAvatar, getAvatar)
import Account.Foundation

-- | Creates a new user. Returns the ID of the created entity. Doesn't set the
-- session value.
newUser :: YesodAccount parent => Text -> Text -> Text
        -> AccountHandler parent (Key (AccountUser parent))
newUser email name pass = do
    sub  <- getYesod
    salt <- randomSalt
    let !img = genIdenticon (acAvatarSprite sub) email
    lift $ runDB $ do
        avatarId <- newAvatar img
        initUser email name (saltedHash salt pass) salt avatarId >>= insert

-- | Checks the given credentials without setting the session value.
-- Returns the user ID if succeed. Tries with the username then the email.
validateUser :: YesodAccount parent => Text -> Text
             -> YesodDB parent (Maybe (Key (AccountUser parent)))
validateUser name pass = do
    app <- lift getYesod
    -- Tries by username, then by email.
    runMaybeT $ do
        Entity userId user  <-     getValidUser (usernameLookup app)
                               <|> getValidUser (emailLookup    app)

        let salt   = accountSalt     app user
            salted = accountPassword app user
        MaybeT $ return $ if saltedHash salt pass == salted
            then Just userId
            else Nothing
  where
    getValidUser unique = MaybeT $ getBy $ unique name

-- | Sets the session value to the given user ID.
setUserId :: YesodAccount parent =>
             Key (AccountUser parent) -> ParentHandler parent ()
setUserId = (sessionKey `setSession`) . T.pack . show

-- | Returns the user's key from the user's session. Does NOT check if the key
-- exists in the database.
getUserId :: YesodAccount parent =>
             ParentHandler parent (Maybe (Key (AccountUser parent)))
getUserId = runMaybeT $ do
    userIdTxt <- MaybeT $ lookupSession sessionKey
    return $ read $ T.unpack userIdTxt

-- | Returns the user entity if the user is authenticated.
getUser :: YesodAccount parent =>
           ParentHandler parent (Maybe (Entity (AccountUser parent), Avatar))
getUser = runMaybeT $ do
    userId <- MaybeT $ getUserId
    MaybeT $ runDB $ runMaybeT $ do -- TODO: Cache
             user   <- MaybeT $ get userId
             avatar <- MaybeT $ getAvatar user
             return (Entity userId user, avatar)

-- | Resets the session value so the user is now no more connected.
unsetUserId :: YesodAccount parent => ParentHandler parent ()
unsetUserId = deleteSession sessionKey

-- | Returns the user entity or invokes a 403 Permission denied if the user
-- isn't authenticated.
requireAuth :: YesodAccount parent =>
               ParentHandler parent (Entity (AccountUser parent), Avatar)
requireAuth = do
    mUser <- getUser

    case mUser of
        Just user -> return user
        Nothing   -> permissionDenied "You are not authenticated"

-- | Returns the user entity. If the user is not authenticated, redirects to
-- the login page.
redirectAuth :: YesodAccount parent =>
                ParentHandler parent (Entity (AccountUser parent), Avatar)
redirectAuth = do
    mUser <- getUser

    case mUser of
        Just user -> return user
        Nothing   -> do
            parent <- getYesod
            setUltDestCurrent
            case authRoute parent of
                Just rte -> redirect rte
                Nothing  -> permissionDenied "Please configure authRoute"

-- | Redirects to 'signInDest' if the user is authenticated.
redirectNoAuth :: YesodAccount parent => ParentHandler parent ()
redirectNoAuth = do
    mUser <- getUser
    case mUser of
        Just _  -> getYesod >>= redirect . signInDest
        Nothing -> return ()

-- Taken from Yesod.Auth -------------------------------------------------------

-- | Generate random salt. Length of 8 is chosen arbitrarily
randomSalt :: MonadIO m => m Text
randomSalt = T.pack `liftM` liftIO (replicateM 8 (randomRIO ('0','z')))

-- | Calculate salted hash using SHA1.
saltedHash :: Text              -- ^ Salt
           -> Text              -- ^ Password
           -> Text
saltedHash salt = T.pack . showDigest . sha1 . C.pack . T.unpack . T.append salt

-- -----------------------------------------------------------------------------
