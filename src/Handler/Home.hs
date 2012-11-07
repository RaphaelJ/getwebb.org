{-# LANGUAGE OverloadedStrings #-}
module Handler.Home (getHomeR)
    where

import Import

import Handler.Upload (uploadForm')

-- Shows the home page
getHomeR :: Handler RepHtml
getHomeR = do
    ((widgetFiles, widgetOpt), enctype) <- generateFormPost uploadForm'

    defaultLayout $ do
        setTitle "getwebb - Free file sharing"
        addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.8.2/jquery.min.js"
        $(widgetFile "homepage")