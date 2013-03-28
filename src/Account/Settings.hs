{-# LANGUAGE OverloadedStrings #-}
-- | The handlers in this modules privides view for users to manager their
-- settings.
module Account.Settings (getSettingsR, postSettingsR) where

import Prelude

import Control.Applicative
import Data.Monoid
import System.Directory (removeFile)
import System.FilePath ((</>))
import System.IO (hClose, openTempFile)

import Yesod
import qualified Vision.Image as I
import qualified Vision.Primitive as I

import Account.Foundation
import Account.Avatar (avatarRoute, hashImage)
import Account.Util (redirectAuth)
import Settings (widgetFile)

data AvatarResult = AvatarResult {
      arPersonalAvatar :: Bool, arFile :: Maybe FileInfo
    }

-- | Displays the sign in and the register forms in the default layout.
getSettingsR :: YesodAccount master => GHandler Account master RepHtml
getSettingsR = do
    (Entity _ user, avatar) <- redirectAuth
    (widget, enctype) <- generateFormPost $ settingsForm user avatar
    displaySettings widget enctype

postSettingsR :: YesodAccount master => GHandler Account master RepHtml
postSettingsR = do
    (Entity _ user, avatar) <- redirectAuth
    ((res, widget), enctype) <- runFormPost $ settingsForm user avatar

    case res of
        FormSuccess (aRes, setts) -> do
            case aRes of
                AvatarResult True  (Just f) -> do
                    tmpDir <- getYesod >>= avatarsDir >>= return . (</> "tmp")
                    (h, tmpPath) <- liftIO $ openTempFile tmpDir ""
                    _ <- register (removeFile tmpPath)
                    liftIO $ hClose h
                    liftIO $ fileMove f tmpPath

                    img <- liftIO $ I.load tmpPath

                    newAvatar img False
                AvatarResult True  Nothing  ->
                    
                AvatarResult False _
                AvatarResult _     Nothing
            
            getYesod >>= redirect . signInDest
        _ -> return ()

    displaySettings widget enctype

displaySettings :: YesodAccount master => GWidget Account master () -> Enctype
                -> GHandler Account master RepHtml
displaySettings widget enctype = do
    toMaster <- getRouteToMaster
    defaultLayout $ do
        setTitle "Account settings - getwebb"
        $(widgetFile "account-settings")

-- | Generates a form which returns the username and the password.
settingsForm :: YesodAccount master =>
                AccountUser master -> (Avatar, AvatarFile) -> Html
             -> MForm Account master (FormResult (AvatarResult
                                                 , AccountSettings master)
                                     , GWidget Account master ())
settingsForm user avatar extra = do
    app <- lift getYesod
    let avatarRte = avatarRoute app (snd avatar)
        generated = avatarGenerated (fst avatar)

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
