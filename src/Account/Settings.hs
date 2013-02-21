{-# LANGUAGE OverloadedStrings #-}
-- | The handlers in this modules privides view for users to manager their
-- settings.
module Account.Settings (getSettingsR, postSettingsR) where

import Prelude

import Yesod

import Account.Foundation
import Account.Util (redirectAuth)

-- | Displays the sign in and the register forms in the default layout.
getSettingsR :: YesodAccount master => GHandler Account master RepHtml
getSettingsR = do
    redirectAuth

data AccountSettings = AccountSettings {
      asAvatar :: Bool, asAvatarFile :: Maybe FileInfo, asPrivacyPublic :: Bool
    }

-- | Generates a form which returns the username and the password.
settingsForm :: YesodAccount master =>
                AccountUser master -> Html
             -> MForm Account master (FormResult AccountSettings
                                     , GWidget Account master ())
settingsForm user html = do
    let form = AccountSettings <$> areq checkBoxField avatarSettings  Nothing
                               <*> aopt fileField     fileSettings    Nothing
                               <*> areq checkBoxField privacySettings Nothing
    (res, widget) <- renderDivs form html

    -- Checks the validity of the credentials.
    case res of
        FormSuccess (name, pass) -> do
            mUserId <- lift $ validateUser name pass
            return $! case mUserId of
                Just userId -> (FormSuccess userId, widget)
                Nothing ->
                    let msg = "Invalid username/email or password." :: Text
                        widget' = [whamlet|
                            <p .errors>#{msg}
                            ^{widget}
                        |]
                    in (FormFailure [msg], widget')
        FormFailure errs -> return (FormFailure errs, widget)
        FormMissing -> return (FormMissing, widget)
  where
    avatarSettings =
        let name = Just "username"
        in FieldSettings {
              fsLabel = "Username or email address", fsTooltip = Nothing
            , fsId = name, fsName = name, fsAttrs = []
            }

    passwordSettings =
        let name = Just "password"
        in FieldSettings {
              fsLabel = "Password", fsTooltip = Nothing, fsId = name
            , fsName = name, fsAttrs = []
            }
