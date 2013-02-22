{-# LANGUAGE OverloadedStrings #-}
-- | The handlers in this modules privides view for users to manager their
-- settings.
module Account.Settings (getSettingsR, postSettingsR) where

import Prelude

import Yesod
import Control.Applicative

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

data AccountSettings = AccountSettings {
      asAvatar :: Bool, asAvatarFile :: Maybe FileInfo, asPrivacyPublic :: Bool
    }

-- | Generates a form which returns the username and the password.
settingsForm :: YesodAccount master => AccountUser master -> Html
             -> MForm Account master (FormResult AccountSettings
                                     , GWidget Account master ())
settingsForm user = renderDivs $
        AccountSettings <$> areq checkBoxField avatarSettings  Nothing
                        <*> aopt fileField     fileSettings    Nothing
                        <*> areq checkBoxField privacySettings Nothing
  where
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

    privacySettings =
        let name = Just "privacy"
        in FieldSettings {
              fsLabel = "Default upload privacy", fsTooltip = Nothing
            , fsId = name, fsName = name, fsAttrs = []
            }
