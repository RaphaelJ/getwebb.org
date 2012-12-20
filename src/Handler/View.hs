{-# LANGUAGE OverloadedStrings #-}
module Handler.View (getViewR)
    where

import Import

import Handler.Upload (uploadForm')
import Handler.Utils (PrettyFileSize (..))

-- Shows information about a file
getViewR :: Text -> Handler RepHtml
getViewR hmac = do
    ((widgetFiles, widgetOpt), enctype) <- generateFormPost uploadForm'

    extras <- getExtra
    let maxFileSize = extraMaxFileSize extras
    let maxRequestSize = extraMaxRequestSize extras

    defaultLayout $ do
        setTitle "getwebb | Free file sharing"
        addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.8.2/jquery.min.js"
        $(widgetFile "homepage")