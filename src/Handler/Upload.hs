{-# LANGUAGE OverloadedStrings #-}
module Handler.Upload where

import Control.Arrow

import Import

data Options = Options {
      optEmail :: Maybe Text
    }
    deriving (Show, Read)

-- | Creates a form for the upload\'s options.
uploadForm :: Text -- ^ The prefix which will precede each field name and id.
           -> Html
           -- | Returns two widgets. The first one is for the files selector
           -- widget and the second for the options form widget.
           -> MForm App App (FormResult ([FileInfo], Options), (Widget, Widget))
uploadForm prefix extra = do
    tell Multipart

    -- File widget
    let filesId = prefix <> "_files"
    let filesView = FieldView {
          fvLabel = toHtml "Select some files to upload"
        , fvTooltip = Nothing, fvId = filesId
        , fvInput = [whamlet|
                <input ##{fileId} name=#{fileId} type=file multiple tabindex=1
                                  autofocus>
            |]
        , fvErrors = Nothing, fvRequired = True
        }
    let filesWidget = [whamlet|
            ^{fvInput filesView}
            ^{extra}
            |]

    -- Retrieve the list of uploaded files
    files <- elems <$> askFiles

    -- Options widget
    let emailId = Just (prefix <> "_email")
    let emailSettings = FieldSettings {
          fsLabel = "Send link by email", fsTooltip = Nothing
        , fsId = emailId, fsName = emailId, fsAttrs = []
    }
    (emailRes, emailView) <- mopt emailField settings Nothing

    let optWidget = [whamlet|
            <label for=^{toHtml $ fvId emailView}>^{fvLabel emailView}:
            ^{fvInput emailView}
            |]
    return ((files, Options <$> emailRes), (filesWidget, optWidget))

-- Uploads files to the server. Returns a Json object which contains the
-- id of the upload or the error.
postUploadR :: Handler RepJson
postUploadR = do
    ((res, (widgetFiles, widgetOpt)), enctype) <- runFormPost uploadForm
    (a, b) <- runRequestBody
    let b' = map (\(x, y) -> (x, fileName y, fileContentType y)) b
    
    case res of
         FormSuccess

    jsonToRepJson $ optEmail res