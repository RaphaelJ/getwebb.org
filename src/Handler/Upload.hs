{-# LANGUAGE OverloadedStrings #-}
module Handler.Upload (Options (..), postUploadR, uploadForm)
    where

import Import

import Control.Monad.Writer
import Data.Map (elems)

import Network.HTTP.Types.Status (
      created201, badRequest400, forbidden403, requestEntityTooLarge413
    )

import Upload.Processing (UploadError (..), processFile)

data Options = Options {
      optPublic :: Bool
    , optEmail  :: Maybe Text
    } deriving (Show, Read)

-- | Uploads files to the server. Returns a Json object which contains the
-- id and the link of the upload or the errors.
postUploadR :: Handler ()
postUploadR = do
    admiKey <- getAdminKey
    ((res, _), _) <- runFormPostNoToken uploadForm
    case res of
        FormSuccess ~(file:_, Options public email) -> do
            eUpload <- processFile admiKey file public
            case eUpload of
                Right upload -> do
                    {- TODO: email -}
                    rep <- jsonToRepJson $ object [
                              "id"            .= uploadHmac upload
                            ]
                    sendResponseStatus created201 rep
                Left err -> do
                    let status = case err of
                            DailyIPLimitReached -> forbidden403
                            FileTooLarge -> requestEntityTooLarge413
                    rep <- jsonToRepJson $ array [show err]
                    sendResponseStatus status rep
        FormFailure errs -> do
            rep <- jsonToRepJson $ array errs
            sendResponseStatus badRequest400 rep
        FormMissing -> undefined

-- | Creates a form for the upload and its options.
uploadForm :: Html
           -- | Returns two widgets. The first one is for the files selector
           -- widget and the second for the options form widget.
           -> MForm App App (FormResult ([FileInfo], Options), (Widget, Widget))
uploadForm extra = do
    tell Multipart

    -- File widget.
    let filesId = "files" :: Text
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

    -- Options.
    let publicId = Just "public"
        publicSettings = FieldSettings {
              fsLabel = "Share this file"
            , fsTooltip = Just "Publish this file in the public gallery."
            , fsId = publicId, fsName = publicId, fsAttrs = []
            }
    (publicRes, publicView) <- mreq checkBoxField publicSettings (Just True)
    liftIO $ print publicRes

    let emailId = Just "email"
        emailSettings = FieldSettings {
              fsLabel = "Send link by email"
            , fsTooltip = Just "Send the link to the uploaded file by email."
            , fsId = emailId, fsName = emailId
            , fsAttrs = [("placeholder", "Enter an email or leave empty")]
            }
    (emailRes, emailView) <- mopt emailField emailSettings Nothing

    let optsWidget = [whamlet|
            <div id="^{toHtml $ fvId publicView}_div">
                ^{fvInput publicView}
                <label for=^{toHtml $ fvId publicView}>^{fvLabel publicView}

            <div id="^{toHtml $ fvId emailView}_div"> 
                <label for=^{toHtml $ fvId emailView}>^{fvLabel emailView}
                ^{fvInput emailView}
            |]

    -- Combines the results of the form.
    let res = (,) <$> filesRes <*> (Options <$> publicRes <*> emailRes)

    return (res, (filesWidget, optsWidget))