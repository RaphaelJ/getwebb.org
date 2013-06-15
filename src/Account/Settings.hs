{-# LANGUAGE BangPatterns, OverloadedStrings, ScopedTypeVariables #-}
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
getSettingsR :: YesodAccount parent => AccountHandler parent Html
getSettingsR = do
    (Entity _ user, avatar) <- lift redirectAuth
    (widget, enctype) <- lift $ generateFormPost $ settingsForm user avatar
    displaySettings user avatar widget enctype

postSettingsR :: YesodAccount parent => AccountHandler parent Html
postSettingsR = do
    (Entity userId user, avatar) <- lift redirectAuth
    ((res, widget), enctype) <- lift $ runFormPost $ settingsForm user avatar

    app <- lift getYesod
    sub <- getYesod
    widget' <- lift $ case res of
        FormSuccess (AvatarResult True (Just f), setts) -> do
            -- New user uploaded avatar.
            let tmpDir = avatarsDir app </> "tmp"
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
        FormSuccess (AvatarResult False _, setts)
            | not (avatarGenerated avatar) -> do
            -- Restores the generated avatar.
            let !img = genIdenticon (acAvatarSprite sub) (accountEmail app user)
            runDB $ do
                replaceAvatar userId user avatar img
                accountSettingsSave userId setts
            redirectSignIn
        FormSuccess (_, setts) -> do
            runDB $ accountSettingsSave userId setts
            redirectSignIn
        _ -> return widget

    displaySettings user avatar widget' enctype
  where
    replaceAvatar userId user oldAvatar img = do
        oldAvatarId <- lift $ getAvatarId user
        removeAvatar oldAvatarId oldAvatar

        app <- getYesod
        avatarId <- newAvatar img
        update userId [ accountAvatarIdField app =. avatarId ]

    redirectSignIn = getYesod >>= redirect . signInDest

displaySettings :: YesodAccount parent => AccountUser parent -> Avatar
                -> ParentWidget parent () -> Enctype
                -> AccountHandler parent Html
displaySettings user avatar widget enctype = do
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
