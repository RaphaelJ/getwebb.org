-- | The handlers in this modules manage the sign in and the register of new
-- accounts.
module Account.Auth (
      getAuthR, getSignInR, getRegisterR
    , postSignInR, postRegisterR
    , signInForm, registerForm
    ) where

import Prelude
import Control.Applicative  as Import ((<$>), (<*>))
import Data.Text as Import (Text)
import qualified Data.Text as T

import Yesod

import Account.Foundation
import Account.Util (requireNoAuth, getUser, randomSalt, saltedHash)

-- | Displays the sign in and the register forms in the default layout.
getAuthR, getSignInR, getRegisterR :: AccountHandler RepHtml
getAuthR = do
    requireNoAuth
    signIn <- generateFormPost signInForm
    register <- generateFormPost registerForm

    showForm signIn register

getSignInR   = getAuthR
getRegisterR = getAuthR

postSignInR :: AccountHandler RepHtml
postSignInR = do
    requireNoAuth
    ((signInRes, signInWidget), signInEnctype) <- runFormPost signInForm
    register <- generateFormPost registerForm

    showForm (signInWidget, signInEnctype) register

postRegisterR :: AccountHandler RepHtml
postRegisterR = do
    requireNoAuth
    signIn <- generateFormPost signInForm
    ((registerRes, registerWidget), registerEnctype) <- runFormPost registerForm

    showForm signIn (registerWidget, registerEnctype)

-- | Responds with the sign in and the register forms in the default layout.
showForm :: YesodAccount master =>
            (GWidget Account master (), Enctype)
         -> (GWidget Account master (), Enctype)
         -> GHandler Account master RepHtml
showForm (signInWidget, signInEnctype) (registerWidget, registerEnctype) = do
    toMaster <- getRouteToMaster
    defaultLayout $ do
        setTitle "Authentication - getwebb"
        $(whamletFile "templates/auth.hamlet")

-- | Generates a form which returns the username and the password.
signInForm :: AccountForm (Text, Text)
signInForm = renderTable $
    (,) <$> areq usernameField  usernameSettings Nothing
        <*> areq passwordField' passwordSettings Nothing
  where
    usernameField = textField
    usernameSettings =
        let name = Just "username"
        in FieldSettings {
              fsLabel = "Username or email address", fsTooltip = Nothing
            , fsId = name, fsName = name, fsAttrs = []
            }

    passwordField' = check checkPassword passwordField
    passwordSettings =
        let name = Just "password"
        in FieldSettings {
              fsLabel = "Password", fsTooltip = Nothing, fsId = name
            , fsName = name, fsAttrs = []
            }
    checkPassword password | T.length password < 6 =
        Left ("Your password must be at least 6 characters long." :: Text)
                           | otherwise             = Right password

data RegisterRes = RegisterRes {
      rrEmail :: Text, rrUsername :: Text, rrPassword :: Text, rrConfirm :: Text
    }

registerForm :: AccountForm (Text, Text, Text)
registerForm html = do
    let form = RegisterRes <$> areq emailField'    emailSettings    Nothing
                           <*> areq usernameField  usernameSettings Nothing
                           <*> areq passwordField' passwordSettings Nothing
                           <*> areq passwordField  passwordSettings Nothing
    (res, widget) <- renderTable form html

    -- Checks if both passwords matche.
    return $! case res of
        FormSuccess (RegisterRes email username pass confirm)
            | pass /= confirm ->
                let msg = "Your passwords don't match." :: Text
                    widget' = [whamlet|
                        <p .errors>#{msg}
                        ^{widget}
                    |]
                in (FormFailure [msg], widget')
            | otherwise -> (FormSuccess (email, username, pass), widget)
        FormFailure errs -> (FormFailure errs, widget)
        FormMissing -> (FormMissing, widget)
  where
    emailField' = checkM checkEmail emailField
    emailSettings =
        let name = Just "email"
        in FieldSettings {
              fsLabel = "Email address", fsTooltip = Nothing
            , fsId = name, fsName = name, fsAttrs = []
            }
    checkEmail email =
        checkExists emailLookup email
                    "This email is already used by another user."

    usernameField = checkM checkUsername textField
    usernameSettings =
        let name = Just "username"
        in FieldSettings {
              fsLabel = "Username", fsTooltip = Nothing
            , fsId = name, fsName = name, fsAttrs = []
            }
    checkUsername, checkEmail :: YesodAccount master =>
            Text -> GHandler Account master (Either Text Text)
    checkUsername username =
        checkExists usernameLookup username
                    "This username is already used by another user."

    -- Returns the error message if the user already exists in the database 
    -- for the given unique lookup key and field value.
    checkExists :: YesodAccount master =>
                   (a -> Unique (AccountUser master)) -> a -> Text
                -> GHandler Account master (Either Text a)
    checkExists unique value errMsg = do
        master <- getYesod
        mUser <- runDB $ getBy $ unique value
        return $! case mUser of
            Just _  -> Left  errMsg
            Nothing -> Right value

    passwordField' = check checkPassword passwordField
    passwordSettings =
        let name = Just "password"
        in FieldSettings {
              fsLabel = "Password"
            , fsTooltip = Just "Minimum 6 characters."
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
