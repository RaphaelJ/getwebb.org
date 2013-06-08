{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}
-- | Defines a set of various functions used in API requests/responses.
module Util.API (
      APIError (..), ToAPIError (..)
    , sendErrorResponse, sendObjectCreated, withFormSuccess
    ) where

import Import

import Data.String (IsString)
import Network.HTTP.Types.Status (Status, badRequest400, created201)

import Util.Hmac (Hmac)

newtype APIError = APIError Text
    deriving (Show, IsString, ToJSON)

class ToAPIError a where
    toAPIError :: a -> APIError

instance ToAPIError APIError where
    toAPIError = id

instance ToAPIError Text where
    toAPIError = APIError

-- | Responds to the client with a status code and a set of errors in an JSON
-- array.
sendErrorResponse :: (MonadHandler m, ToAPIError a) => Status -> [a] -> m b
sendErrorResponse status errs =
    sendResponseStatus status (array $ map toAPIError errs)

-- | Responds to the client with a JSON object containing the hmac of the object
-- and eventually the url to the object.
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

-- | Executes the inner action if the form has been correctly encoded, responds
-- with a 400 Bad request with a JSON array of errors otherwise.
withFormSuccess :: MonadHandler m => FormResult a -> (a -> m b) -> m b
withFormSuccess (FormSuccess a)    f = f a
withFormSuccess (FormFailure errs) _ = sendErrorResponse badRequest400 errs
withFormSuccess FormMissing        _ = 
    sendErrorResponse badRequest400 ["Missing Form." :: Text]
