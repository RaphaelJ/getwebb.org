module Account.Util (
      randomSalt, saltedHash
    ) where

import Prelude

import Control.Monad
import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy.Char8 as BS (pack)
import Data.Text (Text, pack, unpack, append)
import Data.Digest.Pure.SHA (sha1, showDigest)
import System.Random (randomRIO)

import Yesod

import Account.Foundation

-- | Generate random salt. Length of 8 is chosen arbitrarily
randomSalt :: MonadIO m => m Text
randomSalt = pack `liftM` liftIO (replicateM 8 (randomRIO ('0','z')))

-- | Calculate salted hash using SHA1.
saltedHash :: Text              -- ^ Salt
           -> Text              -- ^ Password
           -> Text
saltedHash salt = pack . showDigest . sha1 . BS.pack . unpack . append salt

requireAuth :: AccountHandler ()


getUser :: 
getUser = do
    lookupSession
    