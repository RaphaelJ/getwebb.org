{-# LANGUAGE OverloadedStrings #-}
module Handler.Upload (Options (..), postUploadR, uploadForm, uploadForm')
    where

import Import

import Control.Monad.Writer
import Data.Map (elems)

import Upload.Processing (processFile)

data Options = Options {
      optEmail :: Maybe Text
    }
    deriving (Show, Read)

-- | Uploads files to the server. Returns a Json object which contains the
-- id of the upload or the error.
postUploadR :: Handler RepJson
postUploadR = do
    ((res, _), _) <- runFormPostNoToken uploadForm'

    case res of
        FormSuccess (files@(file:_), Options email) -> do
            processFile file
            jsonToRepJson $! object [ (fileName f, fileContentType f) | f <- files ]
        FormFailure ms -> jsonToRepJson $ object [("errors", array ms)]
        FormMissing -> undefined

-- | Creates a form for the upload and its options.
uploadForm :: Text -- ^ The prefix which will precede each field name and id.
           -> Html
           -- | Returns two widgets. The first one is for the files selector
           -- widget and the second for the options form widget.
           -> MForm App App (FormResult ([FileInfo], Options), (Widget, Widget))
uploadForm prefix extra = do
    tell Multipart

    -- File widget.
    let filesId = prefix <> "files"
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
    let filesRes = case concat <$> elems <$> files of
            Just fs | not (null fs)
                -> FormSuccess fs
            _
                -> FormFailure ["Please send at least one file to upload"]

    -- Options widget.
    let emailId = Just (prefix <> "email")
    let emailSettings = FieldSettings {
          fsLabel = "Send link by email", fsTooltip = Nothing
        , fsId = emailId, fsName = emailId, fsAttrs = []
    }
    (emailRes, emailView) <- mopt emailField emailSettings Nothing

    let optWidget = [whamlet|
            <label for=^{toHtml $ fvId emailView}>^{fvLabel emailView}
            ^{fvInput emailView}
            |]

    -- Combines the results of the form.
    let res = (,) <$> filesRes <*> (Options <$> emailRes)

    return (res, (filesWidget, optWidget))

-- | Same as 'uploadForm' but with upload_ as a predefined prefix.
uploadForm' :: Html
            -> MForm App App (
                    FormResult ([FileInfo], Options), (Widget, Widget)
               )
uploadForm' = uploadForm "upload_"