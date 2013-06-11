{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}
-- | Defines a set of various functions used in API requests/responses.
module Util.API (
      APIError (..), ToAPIError (..)
    , sendObjectCreated, sendNoContent, sendErrorResponse, sendInvalidAdminKey
    , withFormSuccess, withUploadOwner
    ) where

import Import

import Data.String (IsString)
import Network.HTTP.Types.Status (
      Status, badRequest400, created201, noContent204, forbidden403
    )

import Util.Hmac (Hmac)

newtype APIError = APIError Text
    deriving (Show, IsString, ToJSON)

class ToAPIError a where
    toAPIError :: a -> APIError

instance ToAPIError APIError where
    toAPIError = id

instance ToAPIError Text where
    toAPIError = APIError

-- | Responds to the client with a 201 Created and a JSON object containing the
-- HMAC of the object and eventually the URL to the object.
sendObjectCreated :: MonadHandler m =>
                     Hmac -> Maybe (Hmac -> Route (HandlerSite m)) -> m a
sendObjectCreated hmac (Just route) = do
    url <- getUrlRender <*> pure (route hmac)
    addHeader "Location" url
    sendResponseStatus created201 $ object [
            "id" .= hmac, "url" .= url
        ]
sendObjectCreated hmac Nothing      =
    sendResponseStatus created201 $ object [ "id" .= hmac ]

-- | Responds to the client with a 204 created indicating a successful
-- operation.
sendNoContent :: MonadHandler m => m a
sendNoContent = sendResponseStatus noContent204 ()

-- | Responds to the client with a status code and a set of errors in an JSON
-- array.
sendErrorResponse :: (MonadHandler m, ToAPIError a) => Status -> [a] -> m b
sendErrorResponse status errs =
    sendResponseStatus status (array $ map toAPIError errs)

-- | Responds to the client with a 403 status code and a JSON array containing
-- the error saying that he has no ownership on the object.
sendInvalidAdminKey :: MonadHandler m => m a
sendInvalidAdminKey =
    let msg = "You are not the owner of this resource." :: Text
    in sendErrorResponse forbidden403 [msg]

-- | Executes the inner action if the form has been correctly encoded, responds
-- with a 400 Bad request with a JSON array of errors otherwise.
withFormSuccess :: MonadHandler m => FormResult a -> (a -> m b) -> m b
withFormSuccess (FormSuccess a)    f = f a
withFormSuccess (FormFailure errs) _ = sendErrorResponse badRequest400 errs
withFormSuccess FormMissing        _ = 
    sendErrorResponse badRequest400 ["Missing Form." :: Text]

-- | Executes the inner database transaction and then the action if the current
-- user is an admin of the upload. Returns a 403 status code if the user is not
-- the owner of the upload and a 404 status code if the upload doesn't exists.
withUploadOwner :: Hmac -> Handler b -> (Entity Upload -> YesodDB App a)
                -> Handler b
withUploadOwner hmac onSuccess transac = do
    mAdminKey <- getAdminKey

    case mAdminKey of
        Just _  -> do
            success <- runDB $ do
                entity@(Entity _ upload) <- getBy404 $ UniqueUploadHmac hmac

                if isAdmin upload mAdminKey then transac entity >> return True
                                            else return False

            if success then onSuccess
                        else sendInvalidAdminKey
        Nothing -> sendInvalidAdminKey
    