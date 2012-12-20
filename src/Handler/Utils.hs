module Handler.Utils (PrettyFileSize (..))
    where

import Import

import Data.Word
import Text.Printf (printf)

import Text.Blaze (ToMarkup (..))

-- | A new type to represents file size which will be displayed in a human
-- readable way. This type is an instance of 'ToMarkup' and so can be used with
-- the 'toHtml' function.
newtype PrettyFileSize = PrettyFileSize Word64

instance Show PrettyFileSize where
    show (PrettyFileSize size)
        | size < 2    = printf "%d byte" size
        | size < pow1 = printf "%d bytes" size
        | size < pow2 = printf "%d KiB" (size `quot` pow1)
        | size < pow3 = printf "%d MiB" (size `quot` pow2)
        | size < pow4 = printf "%d GiB" (size `quot` pow3)
        | otherwise   = printf "%d TiB" (size `quot` pow4)
      where
        (pow1, pow2, pow3, pow4) = (1024, pow1 * 1024, pow2 * 1024, pow3 * 1024)

instance ToMarkup PrettyFileSize where
    toMarkup = toMarkup . show