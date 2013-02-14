module Account.Util (
      getUserId, getUser, requireAuth, requireNoAuth
    , randomSalt, saltedHash
    ) where

import Prelude

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import qualified Data.ByteString.Lazy.Char8 as BS (pack)
import Data.Text (Text, pack, unpack, append)
import Data.Digest.Pure.SHA (sha1, showDigest)
import System.Random (randomRIO)

import Yesod

import Account.Foundation

-- | Returns the user's key from the user's session. Does NOT check if the key
-- exists in the database.
getUserId :: YesodAccount master =>
             GHandler Account master (Maybe (Key (AccountUser master)))
getUserId = runMaybeT $ do
    userIdTxt <- MaybeT $ lookupSession sessionKey
    return $ read $ unpack userIdTxt

-- | Returns the user entity if the user is authenticated, 'Nothing' otherwise.
getUser :: YesodAccount master =>
           GHandler Account master (Maybe (Entity (AccountUser master)))
getUser = runMaybeT $ do
    userId <- MaybeT $ getUserId
    user   <- MaybeT $ runDB $ get userId
    return $! Entity userId user

-- | Returns the user entity. If the user is not authenticated, redirects to
-- the login page.
requireAuth :: YesodAccount master =>
               GHandler Account master (Entity (AccountUser master))
requireAuth = do
    mUser <- getUser

    case mUser of 
        Just user -> return user
        Nothing -> do
            master <- getYesod
            setUltDestCurrent
            case authRoute master of
                Just rte -> redirect rte
                Nothing  -> permissionDenied "Please configure authRoute"

-- | Redirects to 'signInDest' if the user is authenticated.
requireNoAuth :: AccountHandler ()
requireNoAuth = do
    mUser <- getUser
    case mUser of
        Just user -> getYesod >>= redirect . signInDest
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
