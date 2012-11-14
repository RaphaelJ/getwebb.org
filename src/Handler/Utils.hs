module Handler.Utils (PrettyFileSize (..))
    where

import Import

import Data.Word

import Text.Blaze (ToMarkup (..))

-- | A new type to represents file size which will be displayed in a human
-- readable way. This type is an instance of 'ToMarkup' and so can be used with
-- the 'toHtml' function.
newtype PrettyFileSize = PrettyFileSize Word64

instance Show PrettyFileSize where
    show (PrettyFileSize size)
        | size < 2       = show size                  ++ " byte"
        | size < 1024    = show size                  ++ " bytes"
        | size < 1024^.2 = show (size `quot` 1024^.1) ++ " KiB"
        | size < 1024^.3 = show (size `quot` 1024^.2) ++ " MiB"
        | size < 1024^.4 = show (size `quot` 1024^.3) ++ " GiB"
        | otherwise      = show (size `quot` 1024^.4) ++ " TiB"
      where
        (^.) :: Word64 -> Word64 -> Word64
        a ^. b = a ^ b

instance ToMarkup PrettyFileSize where
    toMarkup = toMarkup . show
