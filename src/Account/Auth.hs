-- | The handlers in this modules manage the sign in and the register of new
-- accounts.
module Account.Auth (
      getAuthR, getSignInR, getRegisterR
    , postSignInR, postRegisterR
    , signInForm, registerForm
    ) where

import Import

import Account.Util (randomSalt, saltedHash)

import Account.Foundation

-- | Displays the sign in and the register forms in the default layout.
getAuthR, getSignInR, getRegisterR :: AccountHandler RepHtml
getAuthR = do
    signIn <- generateFormPost signInForm
    register <- generateFormPost registerForm

    showForm signIn register

getSignInR   = getAuthR
getRegisterR = getAuthR

postSignInR :: AccountHandler RepHtml
postSignInR = do
    ((signInRes, signInWidget), signInEnctype) <- runFormPost signInForm
    register <- generateFormPost registerForm

    showForm (signInWidget, signInEnctype) register

postRegisterR :: AccountHandler RepHtml
postRegisterR = do
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

redirectIfConnected :: AccountHandler RepHtml
redirectIfConnected = do
    

-- | Generates a form which returns the username and the password.
signInForm :: AccountForm (Text, Text)
signInForm = renderTable $
    (,) <$> areq textField     loginSettings    Nothing
        <*> areq passwordField passwordSettings Nothing
  where
    loginSettings =
        let name = Just "signin_login"
        in FieldSettings {
              fsLabel = "Username or email address", fsTooltip = Nothing
            , fsId = name, fsName = name, fsAttrs = []
            }
    passwordSettings =
        let name = Just "signin_password"
        in FieldSettings {
              fsLabel = "Password", fsTooltip = Nothing, fsId = name
            , fsName = name, fsAttrs = []
            }

registerForm :: AccountForm (Text, Text, Text)
registerForm = renderTable $
    (\a b c -> (a, b, c)) <$> areq emailField    emailSettings    Nothing
                          <*> areq textField     loginSettings    Nothing
                          <*> areq passwordField passwordSettings Nothing
  where
    emailSettings =
        let name = Just "register_email"
        in FieldSettings {
              fsLabel = "Email address", fsTooltip = Nothing
            , fsId = name, fsName = name, fsAttrs = []
            }
    loginSettings =
        let name = Just "register_login"
        in FieldSettings {
              fsLabel = "Username", fsTooltip = Nothing
            , fsId = name, fsName = name, fsAttrs = []
            }
    passwordSettings =
        let name = Just "register_password"
        in FieldSettings {
              fsLabel = "Password", fsTooltip = Nothing, fsId = name
            , fsName = name, fsAttrs = []
            }
