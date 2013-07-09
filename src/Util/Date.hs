{-# LANGUAGE OverloadedStrings #-}
module Util.Date (
      PrettyDiffTime (..), NominalDiffTime, Rfc822Date (..)
    , getDiffTime
    ) where

import Prelude

import Control.Monad
import Control.Monad.IO.Class
import Data.Time.Clock (
      UTCTime (..), NominalDiffTime, getCurrentTime, diffUTCTime
    )
import Data.Time.Format (formatTime)
import System.Locale (defaultTimeLocale, rfc822DateFormat)

import Util.Pretty (PrettyDiffTime (..))

-- | A new type to wrap a date so it will be displayed in a standard way.
newtype Rfc822Date = Rfc822Date { unRfc822Date :: UTCTime }

instance Show Rfc822Date where
    show = formatTime defaultTimeLocale rfc822DateFormat . unRfc822Date

-- | Returns the difference of time between the current time and the given time.
getDiffTime :: MonadIO m => UTCTime -> m NominalDiffTime
getDiffTime time = (`diffUTCTime` time) `liftM` liftIO getCurrentTime

