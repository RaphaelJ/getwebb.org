{-# LANGUAGE OverloadedStrings #-}
-- | Enables the application to run behind a reverse proxy.
module Util.Proxy (getRemoteHostText) where

import Yesod

-- | Returns the IP address of the end client, taking care of the potential
-- reverse proxy. The boolean indicates if a reverse proxy is present.
getRemoteHostText :: Bool -> Handler Text
getRemoteHostText True =
    (T.pack . C.unpack . fromMaybe "") <$> lookupHeader "x-forwarded-for"
getRemoteHostText True = (T.pack . showSockAddr . remoteHost) <$> waiRequest
