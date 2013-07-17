{-# LANGUAGE OverloadedStrings #-}
-- | Provides form fields and helpers.
module Util.Form (
      IsTextField (..)
    , twitterField
    , checkLength, checkAlphaNum
    ) where

import Prelude
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T

import Yesod
import Text.Shakespeare.Text (st)

import Util.Pretty (PrettyNumber (..))

-- | Class used to allow text based check functions to be used with any text
-- fields.
class IsTextField a where
    fieldText :: a -> Text

instance IsTextField Text where
    fieldText = id

instance IsTextField Textarea where
    fieldText = unTextarea

-- Fields ----------------------------------------------------------------------

-- | A Twitter username, without the starting @.
twitterField :: ( Monad m, RenderMessage (HandlerSite m) Text
                , RenderMessage (HandlerSite m) FormMessage)
             => Field m Text
twitterField = checkAlphaNum $ checkLength 15 textField

-- Check functions -------------------------------------------------------------

-- | Checks that the text field doesn't exceeds the given length.
checkLength :: (Monad m, RenderMessage (HandlerSite m) Text, IsTextField txt)
            => Int -> Field m txt -> Field m txt
checkLength len =
    checkBool ((<= len) . T.length . fieldText) errMsg
  where
    errMsg = [st|This field can't exceed #{PrettyNumber len} characters.|]

-- | Checks that the text field only contains letters, digits and _.
checkAlphaNum :: (Monad m, RenderMessage (HandlerSite m) Text, IsTextField txt)
              => Field m txt -> Field m txt
checkAlphaNum =
    checkBool (T.all (`S.member` validChars) . fieldText)
              ("This field must only contain alphanumeric characters." :: Text)
  where
    validChars = S.fromList $ '_' : ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9']
