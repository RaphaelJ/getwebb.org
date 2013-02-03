{-# LANGUAGE OverloadedStrings #-}
module Handler.Upload (Options (..), postUploadR, uploadForm, uploadForm')
    where

import Import

import Control.Monad.Writer
import Data.Map (elems)
import qualified Data.Text as T

import Network.HTTP.Types.Status (
      created201, badRequest400, forbidden403, requestEntityTooLarge413
    )

import Upload.Processing (processFile)

data Options = Options { optEmail :: Maybe Text }
    deriving (Show, Read)

-- | Uploads files to the server. Returns a Json object which contains the
-- id and the link of the upload or the errors.
postUploadR :: Handler RepJson
postUploadR = do
    urlRdr <- getUrlRender
    ((res, _), _) <- runFormPostNoToken uploadForm'
    case res of
        FormSuccess (file:_, Options email) -> do
            eUpload <- processFile file
            case eUpload of
                Right upload ->
                    sendResponseStatus created201 (uploadJson urlRdr upload)
                Left err ->
                    let status = case err of
                            DailyIPLimitReached -> forbidden403
                            FileTooLarge -> requestEntityTooLarge413
                        response = jsonToRepJson $ array [show err]
                    in sendResponseStatus status response
        FormFailure errs ->
            sendResponseStatus badRequest400 (jsonToRepJson $ array errs)
        FormMissing -> undefined
  where
    -- Constructs a json object with the information of the file.
    uploadJson urlRdr  upload =
        let hmac = uploadHmac upload
        in jsonToRepJson $ object [
              "id"   .= hmac
            , "name" .= uploadName upload
            , "url"  .= urlRdr (ViewR hmac)
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
          fvLabel = "Select some files to upload."
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
                -> FormFailure ["Send at least one file to upload."]

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