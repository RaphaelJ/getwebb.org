{-# LANGUAGE OverloadedStrings #-}
-- | The handlers in this modules privides view for users to manager their
-- settings.
module Account.Settings (getSettingsR, postSettingsR) where

import Prelude

import Yesod
import Control.Applicative
import Data.Monoid

import Account.Foundation
import Account.Util (redirectAuth)
import Settings (widgetFile)

-- | Displays the sign in and the register forms in the default layout.
getSettingsR :: YesodAccount master => GHandler Account master RepHtml
getSettingsR = do
    Entity _ user <- redirectAuth

    (widget, enctype) <- generateFormPost $ settingsForm user

    toMaster <- getRouteToMaster
    defaultLayout $ do
        setTitle "Account settings - getwebb"
        $(widgetFile "settings")

postSettingsR :: YesodAccount master => GHandler Account master RepHtml
postSettingsR = getSettingsR

data AvatarSettings = AvatarSettings {
      asAvatar :: Bool, asAvatarFile :: Maybe FileInfo
    }

-- | Generates a form which returns the username and the password.
settingsForm :: YesodAccount master => AccountUser master -> Html
             -> MForm Account master (FormResult (AvatarSettings
                                                 , AccountSettings master)
                                     , GWidget Account master ())
settingsForm user extra = do
    (cbRes, cbWidget)     <- renderDivs' $ areq checkBoxField avatarSettings
                                                Nothing
    (fileRes, fileWidget) <- renderDivs' $ aopt fileField     fileSettings
                                                Nothing
    (setsRes, setsWidget) <- renderDivs' accountSettingsForm

    let avatarRes = AvatarSettings <$> cbRes <*> fileRes
        widget = do
        [whamlet|
            #{extra}
            ^{cbWidget}
            <div style="margin-left: 30px">
                ^{fileWidget}

            ^{setsWidget}
        |]

    return ((,) <$> avatarRes <*> setsRes, widget)
  where
    renderDivs' :: AForm sub master a -> 
                   MForm sub master (FormResult a, GWidget sub master ())
    renderDivs' aform = renderDivs aform mempty

    avatarSettings =
        let name = Just "avatar"
        in FieldSettings {
              fsLabel = "Use a personalized avatar", fsTooltip = Nothing
            , fsId = name, fsName = name, fsAttrs = []
            }

    fileSettings =
        let name = Just "avatar_file"
        in FieldSettings {
              fsLabel = "Avatar file"
            , fsTooltip = Just "The image will be resized to 60x60 pixels."
            , fsId = name, fsName = name, fsAttrs = []
            }
