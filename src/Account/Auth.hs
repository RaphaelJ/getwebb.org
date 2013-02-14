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
    (,) <$> areq loginField    loginSettings    Nothing
        <*> areq passwordField' passwordSettings Nothing
  where
    loginField = check checkLogin textField
    loginSettings =
        let name = Just "login"
        in FieldSettings {
              fsLabel = "Username or email address", fsTooltip = Nothing
            , fsId = name, fsName = name, fsAttrs = []
            }
    checkLogin password | T.length password < 6 =
        Left ("Your password must be at least 6 characters long." :: Text)
                     | otherwise          = Right password

    passwordField' = check checkPassword passwordField
    passwordSettings =
        let name = Just "password"
        in FieldSettings {
              fsLabel = "Password", fsTooltip = Nothing, fsId = name
            , fsName = name, fsAttrs = []
            }
    checkPassword password | T.length password < 6 =
        Left ("Your password must be at least 6 characters long." :: Text)
                           | otherwise          = Right password

registerForm :: AccountForm (Text, Text, Text)
registerForm = renderTable $
    (\a b c -> (a, b, c)) <$> areq emailField     emailSettings    Nothing
                          <*> areq textField      loginSettings    Nothing
                          <*> areq passwordField' passwordSettings Nothing
  where
    emailSettings =
        let name = Just "email"
        in FieldSettings {
              fsLabel = "Email address", fsTooltip = Nothing
            , fsId = name, fsName = name, fsAttrs = []
            }
    loginSettings =
        let name = Just "login"
        in FieldSettings {
              fsLabel = "Username", fsTooltip = Nothing
            , fsId = name, fsName = name, fsAttrs = []
            }

    passwordField' = check checkPassword passwordField
    passwordSettings =
        let name = Just "password"
        in FieldSettings {
              fsLabel = "Password"
            , fsTooltip = Just "Your 
            , fsId = name, fsName = name, fsAttrs = []
            }
    checkPassword password | T.length password < 6 =
        Left ("Your password must be at least 6 characters long." :: Text)
                           | otherwise             = Right password
