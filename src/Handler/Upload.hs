{-# LANGUAGE OverloadedStrings #-}
module Handler.Upload where

import Control.Monad.Writer
import Data.Map (elems)

import Import

data Options = Options {
      optEmail :: Maybe Text
    }
    deriving (Show, Read)

-- | Creates a form for the upload and its options.
uploadForm :: Text -- ^ The prefix which will precede each field name and id.
           -> Html
           -- | Returns two widgets. The first one is for the files selector
           -- widget and the second for the options form widget.
           -> MForm App App (FormResult ([FileInfo], Options), (Widget, Widget))
uploadForm prefix extra = do
    tell Multipart

    -- File widget.
    let filesId = prefix <> "_files"
    let filesView = FieldView {
          fvLabel = "Select some files to upload"
        , fvTooltip = Nothing, fvId = filesId
        , fvInput = [whamlet|
                <input ##{filesId} name=#{filesId} type=file multiple tabindex=1
                                   autofocus>
            |]
        , fvErrors = Nothing, fvRequired = True
        }
    let filesWidget = [whamlet|
            ^{fvInput filesView}
            ^{extra}
            |]

    -- Retrieves the list of uploaded files.
    files <- askFiles
    let filesRes = case elems <$> files of
         Nothing -> FormFailure ["Please select at least one file to upload"]
         Just [] -> FormFailure ["Please select at least one file to upload"]
         Just fs -> FormSuccess fs

    -- Options widget.
    let emailId = Just (prefix <> "_email")
    let emailSettings = FieldSettings {
          fsLabel = "Send link by email", fsTooltip = Nothing
        , fsId = emailId, fsName = emailId, fsAttrs = []
    }
    (emailRes, emailView) <- mopt emailField emailSettings Nothing

    let optWidget = [whamlet|
            <label for=^{toHtml $ fvId emailView}>^{fvLabel emailView}:
            ^{fvInput emailView}
            |]

    -- Combines the results of the form.
    let res = (,) <$> filesRes <*> (Options <$> emailRes)

    return (res, (filesWidget, optWidget))

uploadForm' = uploadForm "upload_"

-- | Uploads files to the server. Returns a Json object which contains the
-- id of the upload or the error.
postUploadR :: Handler RepJson
postUploadR = do
    ((res, _), _) <- runFormPost uploadForm'

    let rep = case res of
          FormFailure ms -> object [("errors", array ms)]
          FormSuccess (files, Options email) ->
            object [ (fileName f, fileContentType f) | f <- files ]
          FormMissing -> undefined
    jsonToRepJson rep