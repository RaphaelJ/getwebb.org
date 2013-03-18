{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
-- | The handlers in this modules manage the sign in and the register of new
-- accounts.
module Account.Auth (
      getAuthR, getSignInR, getRegisterR, getSignOutR
    , postSignInR, postRegisterR
    ) where

import Prelude
import Control.Applicative  as Import ((<$>), (<*>))
import Control.Monad.Trans.Either (runEitherT, left, right)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T

import Yesod

import Account.Foundation
import Account.Util (
      registerUser, validateUser, setUserId, unsetUserId, redirectNoAuth
    )
import Settings (widgetFile)

-- | Displays the sign in and the register forms in the default layout.
getAuthR, getSignInR, getRegisterR :: YesodAccount master => 
                                      GHandler Account master RepHtml
getAuthR = do
    redirectNoAuth
    signIn <- generateFormPost signInForm
    register <- generateFormPost registerForm

    showForm signIn register

getSignInR   = getAuthR
getRegisterR = getAuthR

-- | Tries to sign in the user.
postSignInR :: YesodAccount master => GHandler Account master RepHtml
postSignInR = do
    redirectNoAuth
    ((res, widget), enctype) <- runFormPost signInForm
    register <- generateFormPost registerForm

    case res of
        FormSuccess userId -> do
            setUserId userId
            getYesod >>= redirect . signInDest
        _ -> showForm (widget, enctype) register

-- | Tries to register the user.
postRegisterR :: YesodAccount master => GHandler Account master RepHtml
postRegisterR = do
    redirectNoAuth
    signIn <- generateFormPost signInForm
    ((res, widget), enctype) <- runFormPost registerForm

    case res of
        FormSuccess (email, name, pass) -> do
            eUserId <- runDB $ runEitherT $ do
                checkExists usernameLookup name
                            "This username is already used by another user."
                checkExists emailLookup email
                            "This email is already used by another user."
                lift $ registerUser email name pass

            case eUserId of
                Right userId -> do
                    setUserId userId
                    getYesod >>= redirect . signInDest
                Left (msg :: Text) ->
                    let widget' = [whamlet|
                            <p .errors>#{msg}
                            ^{widget}
                        |]
                    in showForm signIn (widget', enctype)
        _ -> showForm signIn (widget, enctype)
  where
    -- Returns the error message if the user already exists in the database
    -- for the given unique lookup key and field value.
    checkExists unique value errMsg = do
        app <- lift $ lift getYesod
        mUser <- lift $ getBy $ unique app value
        case mUser of
            Just _  -> left  errMsg
            Nothing -> right ()

-- | Removes the session value so the user is then signed out. Then redirects.
getSignOutR :: YesodAccount master => GHandler Account master ()
getSignOutR = do
    unsetUserId
    getYesod >>= redirect . signOutDest

-- | Responds with the sign in and the register forms in the default layout.
showForm :: YesodAccount master =>
            (GWidget Account master (), Enctype)
         -> (GWidget Account master (), Enctype)
         -> GHandler Account master RepHtml
showForm (signInWidget, signInEnctype) (registerWidget, registerEnctype) = do
    toMaster <- getRouteToMaster
    defaultLayout $ do
        setTitle "Authentication - getwebb"
        $(widgetFile "account-auth")

-- | Generates a form which returns the username and the password.
signInForm :: YesodAccount master => Html
           -> MForm Account master (FormResult (Key (AccountUser master))
                                   , GWidget Account master ())
signInForm html = do
    let form = (,) <$> areq textField     usernameSettings Nothing
                   <*> areq passwordField passwordSettings Nothing
    (res, widget) <- renderDivs form html

    -- Checks the validity of the credentials.
    case res of
        FormSuccess (name, pass) -> do
            mUserId <- lift $ runDB $ validateUser name pass
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
    usernameSettings =
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

data RegisterRes = RegisterRes {
      rrEmail :: Text, rrUsername :: Text, rrPassword :: Text, rrConfirm :: Text
    }

registerForm :: YesodAccount master => Html
             -> MForm Account master (FormResult (Text, Text, Text)
                                     , GWidget Account master ())
registerForm html = do
    let form = RegisterRes <$> areq emailField     emailSettings    Nothing
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
    emailSettings =
        let name = Just "email"
        in FieldSettings {
              fsLabel = "Email address", fsTooltip = Nothing
            , fsId = name, fsName = name, fsAttrs = []
            }

    usernameField = check checkUsername textField
    usernameSettings =
        let name = Just "username"
        in FieldSettings {
              fsLabel = "Username"
            , fsTooltip = Just "Alphanumeric characters."
            , fsId = name, fsName = name, fsAttrs = []
            }
    checkUsername name | T.all (`S.member` validChars) name = Right name
                       | otherwise                          =
        Left ("Username must only contain alphanumeric characters." :: Text)
    validChars = S.fromList $ ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9']

    passwordField' = check checkPassword passwordField
    passwordSettings =
        let name = Just "password"
        in FieldSettings {
              fsLabel = "Password"
            , fsTooltip = Just "Must be at least 6 characters long."
            , fsId = name, fsName = name, fsAttrs = []
            }
    checkPassword password | T.length password < 6 =
        Left ("Your password must be at least 6 characters long." :: Text)
                           | otherwise             = Right password

    confirmSettings =
        let name = Just "confirm"
        in FieldSettings {
              fsLabel = "Password confirmation"
            , fsTooltip = Just "Repeat your password."
            , fsId = name, fsName = name, fsAttrs = []
            }
