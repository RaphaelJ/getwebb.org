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
        | size < 2    = show size               ++ " byte"
        | size < pow1 = show size               ++ " bytes"
        | size < pow2 = show (size `quot` pow1) ++ " KiB"
        | size < pow3 = show (size `quot` pow2) ++ " MiB"
        | size < pow4 = show (size `quot` pow3) ++ " GiB"
        | otherwise   = show (size `quot` pow4) ++ " TiB"
      where
        (pow1, pow2, pow3, pow4) = (1024, pow1 * 1024, pow2 * 1024, pow3 * 1024)

instance ToMarkup PrettyFileSize where
    toMarkup = toMarkup . show
