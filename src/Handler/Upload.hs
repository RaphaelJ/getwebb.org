{-# LANGUAGE OverloadedStrings #-}
module Handler.Upload where

import Control.Arrow

import Import

data Options = Options {
      optEmail :: Maybe Text
    }

optionsForm :: Html -> MForm App App (FormResult Options, Widget)
optionsForm =
    renderDivs $ Options <$> aopt emailField "Send link by email" Nothing

-- Uploads files to the server. Returns a Json object which contains the
-- id of the upload or the error.
postUploadR :: Handler RepJson
postUploadR = do
    ((res, widget), enctype) <- runFormPost optionsForm
    (a, b) <- runRequestBody
    let b' = map (\(x, y) -> (x, fileName y, fileContentType y)) b

    jsonToRepJson $ object [
        (fileName y, fileContentType y) | (_, y) <- b
        ]