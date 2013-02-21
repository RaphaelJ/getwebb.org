module Account.Util (
      registerUser, validateUser, setUserId, getUserId, getUser, unsetUserId
    , requireAuth, redirectAuth, requireNoAuth
    , randomSalt, saltedHash
    ) where

import Prelude

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans (MonadTrans)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import qualified Data.ByteString.Lazy.Char8 as BS (pack)
import Data.Text (Text, pack, unpack, append)
import Data.Digest.Pure.SHA (sha1, showDigest)
import System.Random (randomRIO)

import Yesod

import Account.Foundation

-- | Creates a new user. Returns the ID of the created entity. Doesn't set the
-- session value.
registerUser :: (YesodAccount master, PersistEntityBackend (AccountUser master)
                 ~ PersistMonadBackend (YesodDB sub master)
                , PersistStore (YesodDB sub master)) =>
                Text -> Text -> Text
             -> GHandler sub master (Key (AccountUser master))
registerUser email name pass = do
    salt <- randomSalt

    initUser email name (saltedHash salt pass) salt >>= runDB . insert

-- | Checks the given credentials without setting the session value.
-- Returns the user ID if succeed. Tries with the username then the email.
validateUser :: (YesodAccount master, PersistEntityBackend (AccountUser master)
                 ~ PersistMonadBackend (YesodDB sub master)
                , Functor (YesodDB sub master), Monad (YesodDB sub master)
                , MonadTrans (YesodPersistBackend master)
                , PersistUnique (YesodDB sub master)) =>
                Text -> Text
             -> GHandler sub master (Maybe (Key (AccountUser master)))
validateUser name pass = do
    -- Tries by username, then by email.
    runDB $ runMaybeT $
        MaybeT (getValidUser usernameLookup) <|>
        MaybeT (getValidUser emailLookup)
  where
    getValidUser unique = runMaybeT $ do
        Entity userId user <- MaybeT $ lift (unique name) >>= getBy
        MaybeT $ lift $ do
            salt   <- accountSalt     user
            salted <- accountPassword user
            if saltedHash salt pass == salted
                then return $! Just userId
                else return Nothing

-- | Sets the session value to the given user ID.
setUserId :: YesodAccount master => Key (AccountUser master)
          -> GHandler sub master ()
setUserId userId = sessionKey `setSession` (pack $ show userId)

-- | Returns the user's key from the user's session. Does NOT check if the key
-- exists in the database.
getUserId :: YesodAccount master =>
             GHandler sub master (Maybe (Key (AccountUser master)))
getUserId = runMaybeT $ do
    userIdTxt <- MaybeT $ lookupSession sessionKey
    return $ read $ unpack userIdTxt

-- | Returns the user entity if the user is authenticated, 'Nothing' otherwise.
getUser :: (YesodAccount master, PersistEntityBackend (AccountUser master)
            ~ PersistMonadBackend (YesodDB sub master)
           , PersistStore (YesodDB sub master)) =>
        GHandler sub master (Maybe (Entity (AccountUser master)))
getUser = runMaybeT $ do
    userId <- MaybeT $ getUserId
    user   <- MaybeT $ runDB $ get userId
    return $! Entity userId user

-- | Resets the session value so the user is now no more connected.
unsetUserId :: YesodAccount master => GHandler sub master ()
unsetUserId = deleteSession sessionKey

-- | Returns the user entity or invokes a 403 Permission denied if the user 
-- isn't authenticated.
requireAuth :: (YesodAccount master, PersistEntityBackend (AccountUser master)
                ~ PersistMonadBackend (YesodDB sub master)
               , PersistStore (YesodDB sub master)) =>
               GHandler sub master (Entity (AccountUser master))
requireAuth = do
    mUser <- getUser

    case mUser of
        Just user -> return user
        Nothing   -> permissionDenied "You are not authenticated"

-- | Returns the user entity. If the user is not authenticated, redirects to
-- the login page.
redirectAuth :: (YesodAccount master, PersistEntityBackend (AccountUser master)
                ~ PersistMonadBackend (YesodDB sub master)
               , PersistStore (YesodDB sub master)) =>
               GHandler sub master (Entity (AccountUser master))
redirectAuth = do
    mUser <- getUser

    case mUser of
        Just user -> return user
        Nothing   -> do
            master <- getYesod
            setUltDestCurrent
            case authRoute master of
                Just rte -> redirect rte
                Nothing  -> permissionDenied "Please configure authRoute"

-- | Redirects to 'signInDest' if the user is authenticated.
redirectNoAuth :: (YesodAccount master, PersistEntityBackend (AccountUser master)
                  ~ PersistMonadBackend (YesodDB sub master)
                  , PersistStore (YesodDB sub master)) =>
                 GHandler sub master ()
redirectNoAuth = do
    mUser <- getUser
    case mUser of
        Just _  -> getYesod >>= redirect . signInDest
        Nothing -> return ()

-- Taken from Yesod.Auth -------------------------------------------------------

-- | Generate random salt. Length of 8 is chosen arbitrarily
randomSalt :: MonadIO m => m Text
randomSalt = pack `liftM` liftIO (replicateM 8 (randomRIO ('0','z')))

-- | Calculate salted hash using SHA1.
saltedHash :: Text              -- ^ Salt
           -> Text              -- ^ Password
           -> Text
saltedHash salt = pack . showDigest . sha1 . BS.pack . unpack . append salt

-- -----------------------------------------------------------------------------
