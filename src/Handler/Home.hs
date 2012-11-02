{-# LANGUAGE OverloadedStrings #-}
module Handler.Home where

import Import

import Handler.Upload (Options, optionsForm)

-- Shows the home page
getHomeR :: Handler RepHtml
getHomeR = do
    (widget, enctype) <- generateFormPost $ renderDivs $ aopt emailField "Send link by email" Nothing
    
    defaultLayout $ do
        setTitle "getwebb - Free file sharing"
        addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.8.2/jquery.min.js"
        $(widgetFile "homepage")