{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}
-- | Defines a set of various functions used in API requests/responses.
module Util.API (
      APIError (..), ToAPIError (..), sendErrorResponse,  withFormSuccess
    ) where

import Import

import Network.HTTP.Types.Status (Status, badRequest400)

newtype APIError = APIError Text
    deriving (Show, ToString, ToJSON)

class ToAPIError a where
    toAPIError :: a -> APIError

instance ToAPIError APIError where
    toAPIError = id

instance ToAPIError Text where
    toAPIError = APIError

-- | Responds to the client with a status code and a set of errors in an JSON
-- array.
sendErrorResponse :: (MonadHandler m, ToAPIError a) => Status -> [a] -> m b
sendErrorResponse errs = do rep <- jsonToRepJson $ array errs
                            sendResponseStatus badRequest400 rep

-- | Executes the inner action if the form has been correctly encoded, responds
-- with a 400 Bad request with a JSON array of errors otherwise.
withFormSuccess :: MonadHandler m => FormResult a -> (a -> m b) -> m b
withFormSuccess res f = f a
    case res of
        FormSuccess a    -> f a
        FormFailure errs -> sendErrorResponse badRequest400 errs
        FormMissing      -> sendErrorResponse badRequest400 ["Missing Form."]
