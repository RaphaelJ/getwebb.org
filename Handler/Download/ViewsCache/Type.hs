{-# LANGUAGE BangPatterns #-}
module Handler.Download.ViewsCache.Type where

import Prelude
import Control.Concurrent (MVar)
import qualified Data.Map as M
import Data.Time.Clock (UTCTime)
import Data.Word

import Model

-- | Temporary cache for the count of views, the last view and the bandwidth
-- which have not been commited yet to the database. The second MVar is used to
-- signal that at least a file view has been added to the cache.
data ViewsCache = ViewsCache {
      vcCache    :: MVar (M.Map UploadId ViewsCacheEntry)
    , vcNewEntry :: MVar ()
    }

-- | Contains the buffered values of an upload.
data ViewsCacheEntry = ViewsCacheEntry {
      vbeViews     :: !(Maybe (Word64, UTCTime)) -- ^ Views count and last view.
    , vbeBandwidth :: !Word64
    }
