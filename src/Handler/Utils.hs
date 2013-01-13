{-# LANGUAGE OverloadedStrings #-}
module Handler.Utils (
      WrappedText (wtText, wtMaxLength), wrappedText
    , PrettyNumber (..), PrettyFileSize (..), PrettyDuration (..)
    , PrettyDiffTime (..)
    , splitHmacs, joinHmacs
    ) where

import Prelude

import Data.Char (intToDigit)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (NominalDiffTime)
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
            in if len == 3 then go 1         d (c : ',' : acc)
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

-- | A new type to represent a media duration which will be displayed in a human
-- readable way. The duration is encoded in centisecond
newtype PrettyDuration = PrettyDuration Word64

instance Show PrettyDuration where
    show (PrettyDuration duration)
        | h > 0     = printf "%d h %d min %d.%d s" h m s c
        | m > 0     = printf "%d min %d.%d s"        m s c
        | otherwise = printf "%d.%d s"                 s c
      where
        (second, minute, hour) = (100, second * 60, minute * 60)
        h = duration `quot` hour
        m = (duration `mod` hour)   `quot` minute
        s = (duration `mod` minute) `quot` second
        c = duration `mod` second

instance ToMarkup PrettyDuration where
    toMarkup = toMarkup . show

-- | A new type to represent a eslaped time an human readable way.
newtype PrettyDiffTime = PrettyDiffTime NominalDiffTime

instance Show PrettyDiffTime where
    show (PrettyDiffTime secs)
        | y > 1     = printf "%d years" y
        | y == 1    = "one year"
        | m > 1     = printf "%d months" m
        | m == 1    = "one month"
        | d > 1     = printf "%d days" d
        | d == 1    = "one day"
        | h > 1     = printf "%d hours" h
        | h == 1    = "one hour"
        | mins > 1  = printf "%d minutes" mins
        | mins == 1 = "one minute"
        | s > 1     = printf "%d seconds" s
        | s == 1    = "one second"
        | otherwise = printf "%.2f second" (realToFrac secs :: Double)
      where
        (minute, hour, day, month, year)
            = (60, minute * 60, hour * 24, day * 30, month * 12)
        y, m, d, h, mins, s :: Int
        y = floor $ secs / year
        m = floor $ secs / month
        d = floor $ secs / day
        h = floor $ secs / hour
        mins = floor $ secs / minute
        s = floor $ secs

instance ToMarkup PrettyDiffTime where
    toMarkup = toMarkup . show

-- | Returns a list of hmacs from a list of url string of hmacs separated by
-- commas.
splitHmacs :: Text -> [Text]
splitHmacs = T.split (== ',')

joinHmacs :: [Text] -> Text
joinHmacs = T.intercalate ","
