{-# LANGUAGE OverloadedStrings #-}
module Handler.Home (getHomeR)
    where

import Import

import Text.Julius (rawJS)

import Handler.Upload (uploadForm')
import Utils.Pretty (PrettyFileSize (..))

-- | Shows the home page.
getHomeR :: Handler RepHtml
getHomeR = do
    ((widgetFiles, widgetOpt), enctype) <- generateFormPost uploadForm'

    extras <- getExtra
    let maxFileSize = extraMaxFileSize extras
    let maxRequestSize = extraMaxRequestSize extras

    defaultLayout $ do
        setTitle "getwebb - Free file sharing"
        $(widgetFile "homepage")