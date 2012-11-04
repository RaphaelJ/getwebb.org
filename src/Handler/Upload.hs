{-# LANGUAGE OverloadedStrings #-}
module Handler.Upload where

import Control.Arrow

import Import

data Options = Options {
      optEmail :: Maybe Text
    }

-- | Creates a form for the upload options.
optionsForm :: Html -> MForm App App (FormResult Options, Widget)
optionsForm extra = do
    let settings = FieldSettings {
          fsLabel = "Send link by email", fsTooltip = Nothing
        , fsId = Just "options_email", fsName = Just "options_email"
        , fsAttrs = []
    }
    (emailRes, emailView) <- mopt emailField settings Nothing
    let widget = do
        [whamlet|
<label for=^{toHtml $ fvId emailView}>^{fvLabel emailView}:
^{fvInput emailView}
^{extra}
|]
    return (Options <$> emailRes, widget)

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