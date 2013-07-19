-- | Displays the terms of use of the service.
module Handler.Terms (getTermsR) where

import Import

import Util.Pretty (PrettyFileSize (..), PrettyNumber (..))

-- | Displays the hottest uploads, accepting a @page@ GET parameter.
getTermsR :: Handler Html
getTermsR = do
    extra <- getExtra
    defaultLayout $ do
        setTitle "Terms of use | getwebb"
        $(widgetFile "terms")
