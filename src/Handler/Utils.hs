{-# LANGUAGE OverloadedStrings #-}
module Handler.Utils (
      WrappedText (wtText, wtMaxLength), PrettyNumber (..), PrettyFileSize (..)
    , wrappedText, splitHmacs, joinHmacs
    ) where

import Import
import Prelude (tail)

import Data.Char (intToDigit)
import qualified Data.Text as T
import Data.Word
import Text.Printf (printf)

import Text.Blaze (ToMarkup (..))

-- | A new type to represents a showable object on which the string
-- representation will be truncated if too long.
data WrappedText = WrappedText {
      wtText :: T.Text -- ^ The original value.
    , wtTruncated :: T.Text -- ^ The truncated result of the show function.
    , wtMaxLength :: Int -- ^ The maximum length of the truncated result.
    }

-- | Truncates the text if longer than the given integer.
wrappedText :: T.Text -> Int -> WrappedText
wrappedText text len =
    WrappedText text trunc len
  where
    trunc | T.length text > len = T.take (len - 3) text `T.append` "..."
          | otherwise           = text

instance Show WrappedText where
    show = T.unpack . wtTruncated

instance ToMarkup WrappedText where
    toMarkup = toMarkup . wtTruncated

-- | A new type to represent large numbers with a separator between thousands.
newtype PrettyNumber = PrettyNumber Int

instance Show PrettyNumber where
    show (PrettyNumber n) | n < 0     = '-' : str
                          | n == 0    = "0"
                          | otherwise = str
      where
        str = go (0 :: Int) (abs n) []

        go _   0 acc = acc
        go len i acc =
            let (d, m) = i `divMod` 10
                c = intToDigit m
            in if len == 3 then go 0         d (c : ',' : acc)
                           else go (len + 1) d (c : acc)

instance ToMarkup PrettyNumber where
    toMarkup = toMarkup . show

-- | A new type to represent file size which will be displayed in a human
-- readable way.
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

-- | Returns a list of hmacs from a list of url string of hmacs separated by
-- commas.
splitHmacs :: Text -> [Text]
splitHmacs = T.split (== ',')

joinHmacs :: [Text] -> Text
joinHmacs = T.intercalate ","
