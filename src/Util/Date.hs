{-# LANGUAGE OverloadedStrings #-}
-- | Defines a few functions to deal with dates.
module Util.Date (
      PrettyDiffTime (..), Rfc822Date (..)
    , getDiffTime
    ) where

import Prelude

import qualified Data.Text as T
import Data.Time.Clock (UTCTime (..), NominalDiffTime (..), getCurrentTime)
import Data.Time.Format (formatTime)
import System.Locale (defaultTimeLocale, rfc822DateFormat)

import Data.Aeson (ToJSON)

import Util.Pretty (PrettyDiffTime (..))

-- | A new type to wrap a date so it will be displayed in a standard way.
newtype Rfc822Date = Rfc822Date UTCTime

instance Show Rfc822Date where
    show time = formatTime defaultTimeLocale rfc822DateFormat

instance ToJSON Rfc822Date where
    toJSON = T.pack . show

-- | Returns the difference of time between the current time and the given time.
getDiffTime :: MonadIO m => UTCTime -> m NominalDiffTime
getDiffTime time = (`diffUTCTime` time) `fmap` liftIO getCurrentTime
