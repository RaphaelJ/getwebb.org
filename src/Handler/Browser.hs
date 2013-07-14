-- | Displays the most viewed uploads.
module Handler.Browser (getBrowserR) where

import Import

import Account
import Util.Extras (getUploadsInfo)
import Util.Hmac (Hmac (..))
import Util.Paging (getPageOffset, pagingWidget)
import Util.Pretty (PrettyFileSize (..), PrettyNumber (..), wrappedText)

uploadsPerPage :: Int
uploadsPerPage = 24

-- Handlers --------------------------------------------------------------------

-- | Displays the hottest uploads, accepting a @page@ GET parameter.
getBrowserR :: Handler Html
getBrowserR = do
    app       <- getYesod
    mUser     <- getUser
    mAdminKey <- getAdminKey

    (page, selectOpts) <- getPageOffset uploadsPerPage
    uploads <- runDB $
        selectList [UploadPublic ==. True]
                   (Desc UploadScore : Desc UploadId : selectOpts) >>=
        getUploadsInfo mAdminKey

    defaultLayout $ do
        let currentUserPage = NotUserPage
            userBarWidget   = $(widgetFile "modules/user-bar")
            uploadsWidget   = $(widgetFile "modules/uploads-list")

        setTitle "Most viewed public files | getwebb"
        $(widgetFile "modules/upload-public-private")
        $(widgetFile "modules/upload-remove")
        $(widgetFile "browser")
