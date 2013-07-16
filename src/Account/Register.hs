{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
-- | The handlers in this modules manage the registration form for a new user.
module Account.Register (getRegisterR, postRegisterR, registerForm) where

import Prelude
import Control.Applicative  as Import ((<$>), (<*>))
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Time.Clock (UTCTime, NominalDiffTime, addUTCTime, getCurrentTime)
import Text.Blaze.Renderer.Text (renderMarkup)

import Yesod

import Account.Foundation
import Account.Util (newUser, setUserId, redirectNoAuth)
import Settings (widgetFile)
import Util.Pretty (PrettyDiffTime (..))
import Util.Proxy (getRemoteHostText)

-- | Seconds between two registrations from the same IP.
minRegistrationInterval :: NominalDiffTime
minRegistrationInterval = 5 * 60

-- | Displays the register form in the default layout.
getRegisterR :: YesodAccount parent => AccountHandler parent Html
getRegisterR = do
    lift redirectNoAuth
    lift (generateFormPost registerForm) >>= showForm

-- | Tries to register the user.
postRegisterR :: YesodAccount parent => AccountHandler parent Html
postRegisterR = do
    lift redirectNoAuth
    ((res, widget), enctype) <- lift (runFormPost registerForm)

    case res of
        FormSuccess (email, name, pass, host, created) -> do
            sub <- getYesod
            userId <- lift $ newUser sub email name pass host created
            lift $ do
                setUserId userId
                getYesod >>= redirect . signInDest
        _ -> showForm (widget, enctype)

-- | Responds with the register form in the default layout.
showForm :: YesodAccount parent => (ParentWidget parent (), Enctype)
         -> AccountHandler parent Html
showForm (widget, enctype) = do
    toParent <- getRouteToParent
    lift $ defaultLayout $ do
        setTitle "Register | getwebb"
        $(widgetFile "account-register")

data RegisterRes = RegisterRes {
      rrEmail :: Text, rrUsername :: Text, rrPassword :: Text, rrConfirm :: Text
    }

registerForm :: YesodAccount parent => Html
             -> MForm (ParentHandler parent) ( FormResult ( Text, Text, Text
                                                          , Text, UTCTime)
                                             , ParentWidget parent ())
registerForm html = do
    let form = RegisterRes <$> areq emailField'    emailSettings    Nothing
                           <*> areq usernameField  usernameSettings Nothing
                           <*> areq passwordField' passwordSettings Nothing
                           <*> areq passwordField  confirmSettings  Nothing
    (res, widget) <- renderDivs form html

    -- Checks if both passwords match and if the client hasn't created an
    -- account recently.
    case res of
        FormSuccess (RegisterRes email name pass confirm)
            | pass /= confirm ->
                let msg = "Your passwords don't match." :: Text
                    widget' = [whamlet|
                        <p .errors>#{msg}
                        ^{widget}
                    |]
                in return (FormFailure [msg], widget')
            | otherwise -> lift $ do
                -- Checks if the user hasn't registered a new account recently.
                app      <- getYesod
                host     <- getRemoteHostText $ usesReverseProxy app
                created  <- liftIO getCurrentTime
                let minCreated = addUTCTime (-minRegistrationInterval) created

                runDB $ do
                    mRecent <- selectFirst [ hostnameLookup app ==. host
                                           , createdLookup app  >.  minCreated ]
                                           []

                    case mRecent of
                        Just _  ->
                            let msg = TL.toStrict $ renderMarkup [shamlet|
                                    Please wait
                                    #{PrettyDiffTime minRegistrationInterval}
                                    between the creation of two new accounts.
                                |]
                                widget' = [whamlet|
                                    <p .errors>#{msg}
                                    ^{widget}
                                |]
                            in return (FormFailure [msg], widget')
                        Nothing ->
                            return ( FormSuccess ( email, name, pass, host
                                                 , created)
                                   , widget)
        FormFailure errs -> return (FormFailure errs, widget)
        FormMissing -> return (FormMissing, widget)
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
