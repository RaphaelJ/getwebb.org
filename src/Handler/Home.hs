{-# LANGUAGE OverloadedStrings #-}
module Handler.Home where

import Import

import Handler.Upload (optionsForm)

-- Shows the home page
getHomeR :: Handler RepHtml
getHomeR = do
    (widget, enctype) <- generateFormPost optionsForm

    defaultLayout $ do
        setTitle "getwebb - Free file sharing"
        addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.8.2/jquery.min.js"
        $(widgetFile "homepage")