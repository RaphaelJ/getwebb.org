{-# LANGUAGE OverloadedStrings #-}
-- | Types and functions to display various values in an human readable way.
module Util.Pretty (
      WrappedText (wtText, wtMaxLength), wrappedText
    , PrettyNumber (..), PrettyFileSize (..), PrettyDuration (..)
    , PrettyDiffTime (..)
    ) where

import Prelude

import Data.Char (intToDigit)
import qualified Data.Text as T
import Data.Text.Lazy.Builder (fromText)
import Data.Time.Clock (NominalDiffTime)
import Data.Word
import Text.Printf (printf)

import Text.Blaze (ToMarkup (..))
import Text.Shakespeare.Text (ToText (..))
import Yesod (shamlet)

-- | A new type to represents a showable object on which the string
-- representation will be truncated if too long.
data WrappedText = WrappedText {
      wtText      :: T.Text -- ^ The original value.
    , wtTruncated :: T.Text -- ^ The truncated result of the show function.
    , wtMaxLength :: Int    -- ^ The maximum length of the truncated result.
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

instance ToText WrappedText where
    toText = fromText . wtTruncated

-- | A new type to represent large numbers with a separator between thousands.
newtype PrettyNumber a = PrettyNumber a
    deriving (Eq, Ord, Num)

instance Integral a => Show (PrettyNumber a) where
    show (PrettyNumber n) | n < 0     = '-' : str
                          | n == 0    = "0"
                          | otherwise = str
      where
        str = go (0 :: Int) (abs n) []

        go _   0 acc = acc
        go len i acc =
            let (d, m) = i `divMod` 10
                c = intToDigit $ int m
            in if len == 3 then go 1         d (c : ',' : acc)
                           else go (len + 1) d (c : acc)

instance Integral a => ToMarkup (PrettyNumber a) where
    toMarkup = toMarkup . show

instance Integral a => ToText (PrettyNumber a) where
    toText = toText . show

-- | A new type to represent file size which will be displayed in a human
-- readable way.
newtype PrettyFileSize = PrettyFileSize Word64 deriving (Eq, Ord, Num)

prettyFileSizeHelper :: PrettyFileSize -> (Word64, String)
prettyFileSizeHelper (PrettyFileSize size)
    | size < 2    = (size, "byte")
    | size < pow1 = (size, "bytes")
    | size < pow2 = (size `quot` pow1, "KiB")
    | size < pow3 = (size `quot` pow2, "MiB")
    | size < pow4 = (size `quot` pow3, "GiB")
    | otherwise   = (size `quot` pow4, "TiB")
  where
    (pow1, pow2, pow3, pow4) = (1024, pow1 * 1024, pow2 * 1024, pow3 * 1024)

instance Show PrettyFileSize where
    show size = let (value, unit) = prettyFileSizeHelper size
                in printf "%s %s" (show (PrettyNumber value)) unit

instance ToMarkup PrettyFileSize where
    toMarkup size = let (value, unit) = prettyFileSizeHelper size
                    in [shamlet|
                        <span .value>#{PrettyNumber value}
                        <span .unit>#{unit}
                    |]

instance ToText PrettyFileSize where
    toText = toText . show

-- | A new type to represent a media duration which will be displayed in a human
-- readable way. The duration is encoded in centisecond.
newtype PrettyDuration = PrettyDuration Word64 deriving (Eq, Ord, Num)

instance Show PrettyDuration where
    show (PrettyDuration duration)
        | h > 0     = printf "%d h %d min %d s" h m s
        | m > 0     = printf "%d min %d s"        m s
        | otherwise = printf "%d s"                 s
      where
        (second, minute, hour) = (100, second * 60, minute * 60)
        h = duration `quot` hour
        m = (duration `mod` hour)   `quot` minute
        s = (duration `mod` minute) `quot` second

instance ToMarkup PrettyDuration where
    toMarkup = toMarkup . show

instance ToText PrettyDuration where
    toText = toText . show

-- | A new type to represent a eslaped time an human readable way.
newtype PrettyDiffTime = PrettyDiffTime NominalDiffTime

prettyDiffTimeHelper :: PrettyDiffTime -> (Int, String)
prettyDiffTimeHelper (PrettyDiffTime secs)
    | y > 1     = (y, "years")
    | y == 1    = (1, "year")
    | m > 1     = (m, "months")
    | m == 1    = (1, "month")
    | d > 1     = (d, "days")
    | d == 1    = (1, "day")
    | h > 1     = (h, "hours")
    | h == 1    = (1, "hour")
    | mins > 1  = (mins, "minutes")
    | mins == 1 = (1, "minute")
    | s > 1     = (s, "seconds")
    | otherwise = (s, "second")
  where
    (minute, hour, day, month, year)
        = (60, minute * 60, hour * 24, day * 30, day * 365)
    y, m, d, h, mins, s :: Int
    y = floor $ secs / year
    m = floor $ secs / month
    d = floor $ secs / day
    h = floor $ secs / hour
    mins = floor $ secs / minute
    s = floor $ secs

instance Show PrettyDiffTime where
    show diff = let (value, unit) = prettyDiffTimeHelper diff
                in printf "%d %s" value unit

instance ToMarkup PrettyDiffTime where
    toMarkup diff = let (value, unit) = prettyDiffTimeHelper diff
                in [shamlet|
                    <span .value>#{value}
                    <span .unit>#{unit}
                |]

instance ToText PrettyDiffTime where
    toText = toText . show

int :: Integral a => a -> Int
int = fromIntegral