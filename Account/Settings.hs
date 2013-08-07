{-# LANGUAGE BangPatterns, OverloadedStrings, ScopedTypeVariables #-}
-- | The handlers in this modules privide a way for users to manager their
-- settings.
module Account.Settings (getSettingsR, postSettingsR) where

import Prelude

import Control.Applicative
import Data.Monoid
import System.Directory (removeFile)
import System.FilePath ((</>))
import System.IO (hClose, openTempFile)

import Yesod
import Control.Monad.Trans.Resource (register)
import Vision.Image (F, RGBAImage (..), convert, load)

import Account.Foundation
import Account.Avatar (
      genIdenticon, avatarImage, newAvatar, removeAvatar, getAvatarId, avatarRoute
    )
import Account.Util (redirectAuth)
import Settings (widgetFile)

data AvatarResult = AvatarResult {
      arPersonalAvatar :: Bool, arFile :: Maybe FileInfo
    }

-- | Displays the sign in and the register forms in the default layout.
getSettingsR :: YesodAccount parent => AccountHandler parent Html
getSettingsR = do
    (userEntity@(Entity _ user), avatar) <- lift redirectAuth
    (widget, enctype) <- lift $ generateFormPost $ settingsForm user avatar
    displaySettings userEntity avatar widget enctype

postSettingsR :: YesodAccount parent => AccountHandler parent Html
postSettingsR = do
    (userEntity@(Entity userId user), avatar) <- lift redirectAuth
    ((res, widget), enctype) <- lift $ runFormPost $ settingsForm user avatar

    app <- lift getYesod
    sub <- getYesod
    widget' <- case res of
        FormSuccess (AvatarResult True (Just f), setts) -> do
            -- New user uploaded avatar.
            let tmpDir = avatarsDir app </> "tmp"
            (tmpPath, h) <- liftIO $ openTempFile tmpDir ""
            _ <- register (removeFile tmpPath)
            liftIO $ hClose h
            liftIO $ fileMove f tmpPath

            eImg <- liftIO $ load tmpPath

            case eImg of
                Right img -> do
                    let img' = avatarImage (convert img :: RGBAImage F)
                    lift $ runDB $ do
                        replaceAvatar userId user avatar img'
                        accountSettingsSave userId setts
                    redirectSuccess
                Left _ ->
                    return [whamlet|
                        <p .errors>Invalid image.
                        ^{widget}
                    |]
        FormSuccess (AvatarResult False _, setts)
            | not (avatarGenerated avatar) -> do
            -- Restores the generated avatar.
            let !img = genIdenticon (acAvatarSprite sub) (accountEmail app user)
            lift $ runDB $ do
                replaceAvatar userId user avatar img
                accountSettingsSave userId setts
            redirectSuccess
        FormSuccess (_, setts) -> do
            lift $ runDB $ accountSettingsSave userId setts
            redirectSuccess
        _ -> return widget

    displaySettings userEntity avatar widget' enctype
  where
    -- Removes the previous avatar before changing the reference to the ne
    -- avatar.
    replaceAvatar userId user oldAvatar img = do
        oldAvatarId <- getAvatarId user
        removeAvatar oldAvatarId oldAvatar

        app <- getYesod
        (avatarId, _) <- newAvatar img
        update userId [accountAvatarIdField app =. avatarId]

    redirectSuccess :: AccountHandler parent a
    redirectSuccess = redirect SettingsR

displaySettings :: YesodAccount parent => Entity (AccountUser parent) -> Avatar
                -> ParentWidget parent () -> Enctype
                -> AccountHandler parent Html
displaySettings userEntity avatar widget enctype = do
    toParent <- getRouteToParent
    app <- lift getYesod
    lift $ defaultLayout $ do
        setTitle "Account settings | getwebb"
        $(widgetFile "account-settings")

-- | Generates a form which returns the username and the password.
settingsForm :: (YesodAccount parent
                , HandlerSite (ParentHandler parent) ~ parent) =>
                AccountUser parent -> Avatar -> Html
             -> MForm (ParentHandler parent)
                      (FormResult (AvatarResult, AccountSettings parent)
                      , ParentWidget parent ())
settingsForm user avatar extra = do
    app <- getYesod
    let avatarRte = avatarRoute app avatar
        generated = avatarGenerated avatar

    (avatarRes, avatarWidget) <- renderDivs' $ areq checkBoxField avatarSettings
                                                    (Just $ not generated)
    (fileRes, fileWidget) <- renderDivs' $ aopt fileField fileSettings Nothing

    (setsRes, setsWidget) <- renderDivs' $ accountSettingsForm user

    let widget = [whamlet|
            #{extra}
            <div .avatar_settings>
                <img .avatar alt="Your avatar" src=@{avatarRte} />
                ^{avatarWidget}

                <div #avatar_file_widget>
                    ^{fileWidget}

            <div .settings>
                ^{setsWidget}
        |]

    return ((,) <$> (AvatarResult <$> avatarRes <*> fileRes) 
                <*> setsRes, widget)
  where
    -- Doesn't render twice @extra@
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
