{-# LANGUAGE OverloadedStrings #-}
module Handler.Home where

import Import

-- Shows the home page
getHomeR :: Handler RepHtml
getHomeR = do
    defaultLayout $ do
        setTitle "getwebb - Free file sharing"
        $(widgetFile "homepage")