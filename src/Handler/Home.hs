{-# LANGUAGE OverloadedStrings #-}
module Handler.Home (getHomeR)
    where

import Import

import Data.Text as T
import Text.Julius (rawJS)

import Database.Persist.Sql (rawSql)

import Handler.Download (routeFile)
import Handler.Upload (uploadForm)
import Util.Pretty (PrettyFileSize (..), wrappedText)

-- | Shows the home page.
getHomeR :: Handler RepHtml
getHomeR = do
    ((filesWidget, optsWidget), enctype) <- generateFormPost uploadForm

    topImages <- runDB getTopImages

    extras <- getExtra
    let maxFileSize    = extraMaxFileSize extras
    let maxRequestSize = extraMaxRequestSize extras

    defaultLayout $ do
        setTitle "Free file sharing | getwebb"
        $(widgetFile "home")
  where
    -- Searchs the ten most popular images. Returns a list of uploads and the
    -- URL to their miniatures.
    getTopImages = do
        let sql = T.unlines [
                  "SELECT ??"
                , "FROM Upload AS upload"
                , "INNER JOIN File AS f ON f.id = upload.file"
                , "WHERE f.type = 'Image' AND upload.public = 1"
                , "LIMIT 38;"
                ]
        imgs <- rawSql sql []

        rdr <- lift getUrlRenderParams
        return [ (i, routeFile rdr i Miniature) | Entity _ i <- imgs ]
