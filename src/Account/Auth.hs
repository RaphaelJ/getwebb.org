{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
-- | The handlers in this modules manage the sign in and sign out.
module Account.Auth (getSignInR, getSignOutR, postSignInR, signInForm) where

import Prelude
import Control.Applicative  as Import ((<$>), (<*>))
import Data.Text (Text)

import Yesod

import Account.Foundation
import Account.Util (validateUser, setUserId, unsetUserId, redirectNoAuth)
import Settings (widgetFile)

-- | Displays the sign in form in the default layout.
getSignInR :: YesodAccount parent => AccountHandler parent Html
getSignInR = do
    lift redirectNoAuth
    lift (generateFormPost signInForm) >>= showForm

-- | Tries to sign in the user.
postSignInR :: YesodAccount parent => AccountHandler parent Html
postSignInR = do
    lift redirectNoAuth
    ((res, widget), enctype) <- lift (runFormPost signInForm)

    case res of
        FormSuccess userId -> lift $ do
            setUserId userId
            getYesod >>= redirect . signInDest
        _ -> showForm (widget, enctype)

-- | Removes the session value so the user is then signed out. Then redirects.
getSignOutR :: YesodAccount parent => AccountHandler parent ()
getSignOutR = lift $ do
    unsetUserId
    getYesod >>= redirect . signOutDest

-- | Responds with the sign in and the register forms in the default layout.
showForm :: YesodAccount parent =>
            (ParentWidget parent (), Enctype) -> AccountHandler parent Html
showForm (widget, enctype) = do
    toParent <- getRouteToParent
    lift $ defaultLayout $ do
        setTitle "Sign in | getwebb"
        $(widgetFile "account-signin")

-- | Generates a form which returns the username and the password.
signInForm :: YesodAccount parent => Html
           -> MForm (ParentHandler parent) 
                    (FormResult (Key (AccountUser parent))
                    , ParentWidget parent ())
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
    usernameSettings = FieldSettings {
              fsLabel = "Username or email address", fsTooltip = Nothing
            , fsId = Nothing, fsName = Just "username", fsAttrs = []
            }

    passwordSettings = FieldSettings {
              fsLabel = "Password", fsTooltip = Nothing, fsId = Nothing
            , fsName = Just "password", fsAttrs = []
            }
