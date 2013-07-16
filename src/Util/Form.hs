-- | Provides form fields and helpers.
module Util.Form ()

import Yesod

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
checkLength len field =
    checkBool ((<= len) . T.length . fieldText)
              [shamlet|This field can't exceed #{PrettyNumber len} characters.|]
