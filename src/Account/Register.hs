{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
-- | The handlers in this modules manage the registration form for a new user.
module Account.Register (getRegisterR, postRegisterR, registerForm) where

import Prelude
import Control.Applicative  as Import ((<$>), (<*>))
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T

import Yesod

import Account.Foundation
import Account.Util (newUser, setUserId, redirectNoAuth)
import Settings (widgetFile)

-- | Displays the register form in the default layout.
getRegisterR :: YesodAccount master => GHandler Account master RepHtml
getRegisterR = do
    redirectNoAuth
    generateFormPost registerForm >>= showForm

-- | Tries to register the user.
postRegisterR :: YesodAccount master => GHandler Account master RepHtml
postRegisterR = do
    redirectNoAuth
    ((res, widget), enctype) <- runFormPost registerForm

    case res of
        FormSuccess (email, name, pass) -> do
            runDB (newUser email name pass) >>= setUserId
            getYesod >>= (redirect . signInDest)
        _ -> showForm (widget, enctype)

-- | Responds with the register form in the default layout.
showForm :: YesodAccount master => (GWidget Account master (), Enctype)
         -> GHandler Account master RepHtml
showForm (widget, enctype) = do
    toMaster <- getRouteToMaster
    defaultLayout $ do
        setTitle "Register | getwebb"
        $(widgetFile "account-register")

data RegisterRes = RegisterRes {
      rrEmail :: Text, rrUsername :: Text, rrPassword :: Text, rrConfirm :: Text
    }

registerForm :: YesodAccount master => Html
             -> MForm Account master (FormResult (Text, Text, Text)
                                     , GWidget Account master ())
registerForm html = do
    let form = RegisterRes <$> areq emailField'    emailSettings    Nothing
                           <*> areq usernameField  usernameSettings Nothing
                           <*> areq passwordField' passwordSettings Nothing
                           <*> areq passwordField  confirmSettings  Nothing
    (res, widget) <- renderDivs form html

    -- Checks if both passwords match.
    return $! case res of
        FormSuccess (RegisterRes email name pass confirm)
            | pass /= confirm ->
                let msg = "Your passwords don't match." :: Text
                    widget' = [whamlet|
                        <p .errors>#{msg}
                        ^{widget}
                    |]
                in (FormFailure [msg], widget')
            | otherwise -> (FormSuccess (email, name, pass), widget)
        FormFailure errs -> (FormFailure errs, widget)
        FormMissing -> (FormMissing, widget)
  where
    emailField' = checkM checkEmailExists emailField
    emailSettings = FieldSettings {
          fsLabel = "Email address", fsTooltip = Nothing
        , fsId = Nothing, fsName = Just "email", fsAttrs = []
        }
    checkEmailExists email =
        checkExists emailLookup email
                    ("This email is already used by another user." :: Text)

    usernameField = checkM checkUsernameExists $ check checkUsername textField
    usernameSettings = FieldSettings {
          fsLabel = "Username", fsTooltip = Just "Alphanumeric characters."
        , fsId = Nothing, fsName = Just "username", fsAttrs = []
        }
    checkUsername name | T.any (not . (`S.member` validChars)) name =
        Left ("Username must only contain alphanumeric characters." :: Text)
                       | otherwise                                  = Right name
    validChars = S.fromList $ ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9']
    checkUsernameExists name =
        checkExists usernameLookup name
                    ("This username is already used by another user." :: Text)

    passwordField' = check checkPassword passwordField
    passwordSettings = FieldSettings {
          fsLabel = "Password"
        , fsTooltip = Just "Must be at least 6 characters long."
        , fsId = Nothing, fsName = Just "password", fsAttrs = []
        }
    checkPassword password | T.length password < 6 =
        Left ("Your password must be at least 6 characters long." :: Text)
                           | otherwise             = Right password

    confirmSettings = FieldSettings {
          fsLabel = "Password confirmation"
        , fsTooltip = Just "Repeat your password."
        , fsId = Nothing, fsName = Just "confirm", fsAttrs = []
        }

    -- Returns the error message if the value already exists in the database
    -- for the given unique lookup key, returns the value if doesn't exists.
    checkExists unique value errMsg = runDB $ do
        app <- lift getYesod
        mUser <- getBy $ unique app value
        return $ case mUser of Just _  -> Left errMsg
                               Nothing -> Right value
