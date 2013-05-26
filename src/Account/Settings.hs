{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
-- | The handlers in this modules privide a way for users to manager their
-- settings.
module Account.Settings (getSettingsR, postSettingsR) where

import Prelude

import Control.Applicative
import qualified Control.Exception as E
import Data.Monoid
import Data.Text (Text)
import System.Directory (removeFile)
import System.FilePath ((</>))
import System.IO (hClose, openTempFile)

import Yesod
import Control.Monad.Trans.Resource (register)
import qualified Vision.Image as I

import Account.Foundation
import Account.Avatar (
      genIdenticon, genAvatar, newAvatar, removeAvatar, getAvatarId, avatarRoute
    )
import Account.Util (redirectAuth)
import Settings (widgetFile)
import Util.Pretty (wrappedText)

data AvatarResult = AvatarResult {
      arPersonalAvatar :: Bool, arFile :: Maybe FileInfo
    }

-- | Displays the sign in and the register forms in the default layout.
getSettingsR :: YesodAccount master => GHandler Account master RepHtml
getSettingsR = do
    (Entity _ user, avatar) <- redirectAuth
    (widget, enctype) <- generateFormPost $ settingsForm user avatar
    displaySettings user avatar widget enctype

postSettingsR :: YesodAccount master => GHandler Account master RepHtml
postSettingsR = do
    (Entity userId user, avatar) <- redirectAuth
    ((res, widget), enctype) <- runFormPost $ settingsForm user avatar

    widget' <- case res of
        FormSuccess (ar, setts) -> do
            case ar of
                AvatarResult True (Just f) -> do
                    -- New user uploaded avatar.
                    tmpDir <- getYesod >>= return . (</> "tmp") . avatarsDir
                    (tmpPath, h) <- liftIO $ openTempFile tmpDir ""
                    _ <- register (removeFile tmpPath)
                    liftIO $ hClose h
                    liftIO $ fileMove f tmpPath

                    eImg <- liftIO $ E.try (I.load tmpPath)

                    case eImg of
                        Right img -> do
                            runDB $ do
                                replaceAvatar userId user avatar (genAvatar img)
                                accountSettingsSave userId setts
                            redirectSignIn
                        Left (_ :: E.SomeException) ->
                            let msg = "Invalid image." :: Text
                            in return [whamlet|
                                <p .errors>#{msg}
                                ^{widget}
                            |]
                AvatarResult False _ | not (avatarGenerated avatar) -> do
                    -- Restores the generated avatar.
                    app <- getYesod
                    img <- genIdenticon (accountEmail app user)
                    runDB $ do
                        replaceAvatar userId user avatar img
                        accountSettingsSave userId setts
                    redirectSignIn
                _ -> do
                    runDB $ accountSettingsSave userId setts
                    redirectSignIn
        _ -> return widget

    displaySettings user avatar widget' enctype
  where
    replaceAvatar userId user oldAvatar img = do
        oldAvatarId <- lift $ getAvatarId user
        removeAvatar oldAvatarId oldAvatar

        app <- lift getYesod
        avatarId <- newAvatar img
        update userId [ accountAvatarIdField app =. avatarId ]

    redirectSignIn = getYesod >>= redirect . signInDest

displaySettings :: YesodAccount master => AccountUser master -> Avatar
                -> GWidget Account master () -> Enctype
                -> GHandler Account master RepHtml
displaySettings user avatar widget enctype = do
    toMaster <- getRouteToMaster
    app <- getYesod
    defaultLayout $ do
        setTitle "Account settings | getwebb"
        $(widgetFile "account-settings")

-- | Generates a form which returns the username and the password.
settingsForm :: YesodAccount master =>
                AccountUser master -> Avatar -> Html
             -> MForm Account master (FormResult (AvatarResult
                                                 , AccountSettings master)
                                     , GWidget Account master ())
settingsForm user avatar extra = do
    app <- lift getYesod
    let avatarRte = avatarRoute app avatar
        generated = avatarGenerated avatar

    (avatarRes, avatarWidget) <- renderDivs' $ areq checkBoxField avatarSettings
                                                    (Just $ not generated)
    (fileRes, fileWidget) <- renderDivs' $ aopt fileField fileSettings Nothing

    (setsRes, setsWidget) <- renderDivs' $ accountSettingsForm user

    let widget = [whamlet|
            #{extra}
            <div .avatar_settings>
                <img alt="Your avatar" src=@{avatarRte} />
                ^{avatarWidget}

                <div #avatar_file_widget>
                    ^{fileWidget}

            <div .settings>
                ^{setsWidget}
        |]

    return ((,) <$> (AvatarResult <$> avatarRes <*> fileRes) 
                <*> setsRes, widget)
  where
    renderDivs' :: AForm sub master a -> 
                   MForm sub master (FormResult a, GWidget sub master ())
    renderDivs' aform = renderDivs aform mempty

    avatarSettings =
        let name = Just "avatar_personal"
        in FieldSettings {
              fsLabel = "Use a personalized avatar", fsTooltip = Nothing
            , fsId = name, fsName = name, fsAttrs = []
            }

    fileSettings =
        let name = Just "avatar_file"
        in FieldSettings {
              fsLabel = "Avatar file"
            , fsTooltip = Just "The image will be scaled down to 80x80 pixels."
            , fsId = name, fsName = name, fsAttrs = []
            }
