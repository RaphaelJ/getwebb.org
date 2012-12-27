{-# LANGUAGE OverloadedStrings #-}
module Handler.View (getViewR)
    where

import Import

import Handler.Utils (PrettyFileSize (..), splitHmacs)

-- Shows information about a file
getViewR :: Text -> Handler RepHtml
getViewR hmacsJoined = do
    defaultLayout $ do
        setTitle "getwebb | Free file sharing"
  where
    hmacs@(hmac:_) = splitHmacs hmacsJoined