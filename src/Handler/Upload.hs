{-# LANGUAGE OverloadedStrings #-}
module Handler.Upload (Options (..), postUploadR, uploadForm, uploadForm')
    where

import Import

import Control.Monad.Writer
import Data.Map (elems)
import qualified Data.Text as T

import Upload.Processing (process)

data Options = Options {
      optEmail :: Maybe Text
    }
    deriving (Show, Read)

-- | Uploads files to the server. Returns a Json object which contains the
-- id and the link of the upload or the errors.
postUploadR :: Handler RepJson
postUploadR = do
    urlRender <- getUrlRender
    ((res, _), _) <- runFormPostNoToken uploadForm'
    case res of
        FormSuccess (files, Options email) -> do
            uploads <- process files
            jsonToRepJson $! array $ map (uploadJson urlRender) uploads
        FormFailure errs -> 
            jsonToRepJson $ object [("invalid request", array errs)]
        FormMissing -> undefined
  where
    -- Constructs a json object if uploaded successfully or the error.
    uploadJson _         (Left err) = String $ T.pack $ show err
    uploadJson urlRender (Right upload) =
        let hmac = uploadHmac upload
        in object [
              "id"   .= hmac
            , "name" .= uploadName upload
            , "url"  .= urlRender (ViewR hmac)
            ]

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

-- | Same as 'uploadForm' but without a prefix.
uploadForm' :: Html
            -> MForm App App (
                    FormResult ([FileInfo], Options), (Widget, Widget)
               )
uploadForm' = uploadForm ""