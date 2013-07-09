-- | Displays the user\'s public profile.
module Handler.User (getUserGalleryR, getUserCommentsR)
    where

import Import
import Control.Monad
import Data.Maybe
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Text.Hamlet (shamlet)

import Account
import Handler.Comment (retrieveCommentVote)
import Util.Hmac (Hmac (..))
import Util.Date (getDiffTime)
import Util.Extras (getUploadInfo, getUploadsInfo)
import Util.Pretty (
      WrappedText, PrettyNumber (..), PrettyFileSize (..), PrettyDiffTime (..)
    , wrappedText
    )

-- Handlers --------------------------------------------------------------------

-- | Enumeration used for current position in the navigation menu.
data ProfilePage = PublicGallery | Comments

-- | Displays the profile overview of an user and its public gallery.
getUserGalleryR :: Text -> Handler Html
getUserGalleryR name = do
    app <- getYesod
    mUser <- getUser

    (entity, profileAvatar, profileUploadsCount, uploads) <- runDB $ do
        (entity, profileAvatar, profileUploadsCount) <- getUserProfile name

        -- Fetches the user's gallery.
        let adminKeyId = userAdminKey $ entityVal entity
        uploads' <- selectList [ UploadAdminKey ==. adminKeyId
                               , UploadPublic   ==. True ]
                               [ Desc UploadScore ]
        uploads <- getUploadsInfo uploads'

        return (entity, profileAvatar, profileUploadsCount, uploads)

    defaultLayout $ do
        let Entity profileId profile = entity
        currentTime <- liftIO getCurrentTime

        setTitle [shamlet|Gallery of #{wrappedUserName profile} | getwebb|]
        $(widgetFile "user-gallery")

-- | Displays the profile overview of an user and its comments.
getUserCommentsR :: Text -> Handler Html
getUserCommentsR name = do
    app <- getYesod
    mUser <- getUser
    let mUserId = ((entityKey . fst) <$> mUser)

    (entity, profileAvatar, profileUploadsCount, comments) <- runDB $ do
        (entity, profileAvatar, profileUploadsCount) <- getUserProfile name

        -- Fetches the user's comments and the corresponding upload.
        comments' <- selectList [CommentUser ==. entityKey entity]
                                [Desc CommentId]
        comments  <- forM comments' $ \(Entity commentId comment) -> do
            let uploadId = commentUpload comment
            upload       <- getJust uploadId

            -- Skips non-public uploads.
            if uploadPublic upload then do
                (_, _, icon) <- getUploadInfo (Entity uploadId upload)
                mVote        <- retrieveCommentVote mUserId commentId
                return $ Just (comment, upload, icon, mVote)
                                   else
                return Nothing

        return (entity, profileAvatar, profileUploadsCount, catMaybes comments)

    defaultLayout $ do
        let Entity profileId profile = entity
            isUserProfile            = Just profileId == mUserId
        currentTime <- liftIO getCurrentTime

        setTitle [shamlet|Comments of #{wrappedUserName profile} | getwebb|]
        $(widgetFile "comments")
        $(widgetFile "user-comments")

-- -----------------------------------------------------------------------------

-- | Returns the user, its avatar and its uploads count corresponding to the
-- given user name. Returns a 404 error if the user doesn't exists.
getUserProfile :: Text -> YesodDB App (Entity User, Avatar, Int)
getUserProfile name = do
    entity@(Entity _ profile) <- getBy404 (UniqueUserName name)
    Just profileAvatar        <- getAvatar profile
    adminKey                  <- getJust (userAdminKey profile)
    return (entity, profileAvatar, adminKeyCount adminKey)

--  | Returns the widget with display the profile of @entity@ as seen by user
-- @mUser@.
profileWidget :: ProfilePage -> Maybe (Entity User, Avatar)
              -> (Entity User, Avatar, Int) -> Widget
profileWidget profilePage mUser (entity, profileAvatar, profileUploadsCount) = do
    app <- getYesod

    let Entity profileId profile = entity
        currentUserPage = UserProfile profileId
        userBarWidget   = $(widgetFile "user-bar")
    profileDiffCreated <- getDiffTime $ userCreated profile

    $(widgetFile "user-profile")

-- | Limits the length of the user name.
wrappedUserName :: User -> WrappedText
wrappedUserName profile = wrappedText (userName profile) 40