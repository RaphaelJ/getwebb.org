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
getSignInR :: YesodAccount master => GHandler Account master RepHtml
getSignInR = do
    redirectNoAuth
    generateFormPost signInForm >>= showForm

-- | Tries to sign in the user.
postSignInR :: YesodAccount master => GHandler Account master RepHtml
postSignInR = do
    redirectNoAuth
    ((res, widget), enctype) <- runFormPost signInForm

    case res of
        FormSuccess userId -> do
            setUserId userId
            getYesod >>= redirect . signInDest
        _ -> showForm (widget, enctype)

-- | Removes the session value so the user is then signed out. Then redirects.
getSignOutR :: YesodAccount master => GHandler Account master ()
getSignOutR = do
    unsetUserId
    getYesod >>= redirect . signOutDest

-- | Responds with the sign in and the register forms in the default layout.
showForm :: YesodAccount master => (GWidget Account master (), Enctype)
         -> GHandler Account master RepHtml
showForm (widget, enctype) = do
    toMaster <- getRouteToMaster
    defaultLayout $ do
        setTitle "Sign in | getwebb"
        $(widgetFile "account-signin")

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
    usernameSettings = FieldSettings {
              fsLabel = "Username or email address", fsTooltip = Nothing
            , fsId = Nothing, fsName = Just "username", fsAttrs = []
            }

    passwordSettings = FieldSettings {
              fsLabel = "Password", fsTooltip = Nothing, fsId = Nothing
            , fsName = Just "password", fsAttrs = []
            }
