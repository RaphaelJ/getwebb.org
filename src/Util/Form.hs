-- | Provides form fields and helpers.
module Util.Form (IsTextField (..), checkLength) where

import Prelude
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import Yesod
import Text.Blaze.Renderer.Text (renderMarkup)

import Util.Pretty (PrettyNumber (..))

-- | Class used to allow text based check functions to be used with any text
-- fields.
class IsTextField a where
    fieldText :: a -> Text

instance IsTextField Text where
    fieldText = id

instance IsTextField Textarea where
    fieldText = unTextarea

-- | Checks that the text field doesn't exceeds the given length.
checkLength :: (Monad m, RenderMessage (HandlerSite m) Text, IsTextField txt) =>
               Int -> Field m txt -> Field m txt
checkLength len =
    checkBool ((<= len) . T.length . fieldText) errMsg
  where
    errMsg = TL.toStrict $ renderMarkup [shamlet|
            This field can't exceed #{PrettyNumber len} characters.
        |]
