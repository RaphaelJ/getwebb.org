module Util.Proxy (getRemoteHostText) where

import Prelude
import Control.Applicative ((<$>))
import qualified Data.ByteString.Char8 as C
import Data.Maybe
import qualified Data.Text as T

import Yesod
import Network.Wai (remoteHost)
import Network.Wai.Logger.Utils (showSockAddr)

-- | Returns the IP address of the end client, taking care of the potential
-- reverse proxy. The boolean indicates if a reverse proxy is present.
getRemoteHostText :: MonadHandler m => Bool -> m T.Text
getRemoteHostText True  =
    (T.pack . C.unpack . fromMaybe "") <$> lookupHeader "x-forwarded-for"
getRemoteHostText False = (T.pack . showSockAddr . remoteHost) <$> waiRequest
