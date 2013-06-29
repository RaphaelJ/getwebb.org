{-# LANGUAGE OverloadedStrings #-}
-- | Shows and processes the upload form.
module Handler.Upload (Options (..), getUploadR, postUploadR, uploadForm)
    where

import Import

import Control.Monad.Writer hiding (lift)
import Data.Map (elems)
import qualified Data.Text as T

import Database.Persist.Sql (rawSql)
import Network.HTTP.Types.Status (requestEntityTooLarge413)
import Text.Julius (rawJS)

import Account (getUser)
import Handler.Upload.Processing (UploadError (..), processFile)
import Util.API (
      sendObjectCreated, sendErrorResponse, tooManyRequests429, withFormSuccess
    )
import Util.Hmac (Hmac (..))
import Util.Pretty (PrettyFileSize (..), wrappedText)

-- | Options which can be selected by the user when uploading a file.
data Options = Options {
      optPublic :: Bool
    , optEmail  :: Maybe Text
    } deriving (Show, Read)

-- | Shows the home page with the upload form.
getUploadR :: Handler Html
getUploadR = do
    ((filesWidget, optsWidget), enctype) <- generateFormPost uploadForm

    topImages <- runDB getTopImages

    extras <- getExtra
    let maxFileSize    = extraMaxFileSize extras
    let maxRequestSize = extraMaxRequestSize extras

    defaultLayout $ do
        setTitle "Free file sharing | getwebb"
        $(widgetFile "upload")
  where
    -- Searchs the ten most popular images. Returns a list of uploads and the
    -- URL to their miniatures.
    getTopImages = do
        let sql = T.unlines [
                  "SELECT ??"
                , "FROM Upload AS upload"
                , "INNER JOIN File AS f ON f.id = upload.file"
                , "WHERE f.type = 'Image' AND upload.public = 1"
                , "LIMIT 40;"
                ]
        imgs <- rawSql sql []

        return [ (i, DownloadMiniatureR (uploadHmac i)) | Entity _ i <- imgs ]

-- | Uploads a file to the server. Returns a 201 Created with a JSON object
-- which contains the id of the upload, or a 400/413/429 with a JSON array of
-- errors.
postUploadR :: Handler ()
postUploadR = do
    ((res, _), _) <- runFormPostNoToken uploadForm
    withFormSuccess res $ \ (~(file:_), Options public email) -> do
        -- Allocates an new admin key if the client doesn't have one.
        mAdminKey <- getAdminKey
        adminKeyId <- case mAdminKey of
            Just (AdminKeyUser (Entity i _) _) -> return i
            Just (AdminKeyAnon (Entity i _))   -> return i
            Nothing                            -> do
                adminKeyId <- runDB newAdminKey
                setAdminKey adminKeyId
                return adminKeyId

        eUpload <- processFile adminKeyId file public
        case eUpload of
            Right upload -> do
                {- TODO: email -}
                sendObjectCreated (uploadHmac upload)
                                  (Just (ViewR . toPathPiece))
            Left err -> do
                let status = case err of
                        DailyIPLimitReached -> tooManyRequests429
                        FileTooLarge -> requestEntityTooLarge413
                sendErrorResponse status [err]
  where

-- | Creates a form for the upload and its options.
uploadForm :: Html
           -- | Returns two widgets. The first one is for the files selector
           -- widget and the second for the options\' widget.
           -> MForm Handler (FormResult ([FileInfo], Options), (Widget, Widget))
uploadForm extra = do
    tell Multipart

    -- File selector
    let filesId = "files" :: Text
    let filesWidget = [whamlet|
            <input name=#{filesId} type=file multiple tabindex=1 autofocus>
            ^{extra}
            |]

    -- Retrieves the list of uploaded files.
    files <- askFiles
    let filesRes = case concat <$> elems <$> files of
            Just fs | not (null fs)
                -> FormSuccess fs
            _
                -> FormFailure ["Send at least one file to upload."]

    -- Options form
    defPriv <- maybe True (userDefaultPublic . entityVal . fst) <$> lift getUser
    let publicSettings = FieldSettings {
              fsLabel = "Share this file"
            , fsTooltip = Just "Publish this file in the public gallery."
            , fsId = Nothing, fsName = Just "public", fsAttrs = []
            }
    (publicRes, publicView) <- mreq checkBoxField publicSettings (Just defPriv)

    let emailSettings = FieldSettings {
              fsLabel = "Send link by email"
            , fsTooltip = Just "Send the link to the uploaded file by email."
            , fsId = Nothing, fsName = Just "email"
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
