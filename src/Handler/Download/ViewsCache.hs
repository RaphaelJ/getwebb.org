{-# LANGUAGE BangPatterns #-}
-- | Daemon which stores and updates in memory the number of views, the last
-- view and the bandwidth consumption of an upload, synchronising itself with
-- the database on a regular basis.
-- Enable us to track in real time the number of views and the bandwidth of
-- every upload without updating the database (= disk access) at each file view.
module Handler.Download.ViewsCache (
     module Handler.Download.ViewsCache.Type, commitDelay
   -- * Cache management
   , newCache, incrementViewCount, addBandwidth, getCacheEntry, getTrackedBS
   -- * Starting the daemon
   , viewsCacheDaemon, forkViewsCacheDaemon
   ) where

import Import

import Control.Concurrent (
      ThreadId, forkIO, newMVar, newEmptyMVar, readMVar, modifyMVar_
    , takeMVar, tryPutMVar, swapMVar, threadDelay
    )
import Control.Monad
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Internal as L
import qualified Data.Map as M
import Data.Time.Clock (getCurrentTime)
import System.IO.Unsafe (unsafeInterleaveIO)

import Handler.Download.ViewsCache.Type
import Handler.Upload (score)
import JobsDaemon.Util (runDBIO)

-- | Delay in microseconds at which the views cache is commited to the
-- database.
commitDelay :: Int
commitDelay = 5 * 10^(6 :: Int) -- Every five seconds.

-- | Initialises a new view cache to be inserted in the foundation type.
newCache :: IO ViewsCache
newCache = do
    cache  <- newMVar M.empty
    signal <- newEmptyMVar
    return $ ViewsCache cache signal

-- | Updates the cache to increment by one the number of views and to update the
-- last view date.
incrementViewCount :: App -> UploadId -> IO ()
incrementViewCount app uploadId = do
    let ViewsCache cache signal = viewsCache app

    -- Updates the cache atomically.
    time <- getCurrentTime
    modifyMVar_ cache $ \accum -> do
        return $! M.insertWith (f time) uploadId 
                               (ViewsCacheEntry (Just (1, time)) 0) accum

    -- Signals to the daemon that at least an entry has been added to the
    -- cache.
    _ <- signal `tryPutMVar` ()
    return ()
  where
    -- Increments the number of views and the last view if one entry exists.
    f !time _ (ViewsCacheEntry (Just (c, _)) bw) =
        let !c' = c + 1
        in ViewsCacheEntry (Just (c', time)) bw
    f !time _ (ViewsCacheEntry Nothing       bw) =
        ViewsCacheEntry (Just (1, time)) bw

-- | Adds the number of bytes to the uncommited amount of bandwidth.
-- Doesn't imply a new view as the view count is only incremented on download
-- completion.
addBandwidth :: App -> UploadId -> Word64 -> IO ()
addBandwidth app uploadId !len = do
    let ViewsCache cache signal = viewsCache app

    -- Updates the cache atomically.
    liftIO $ modifyMVar_ cache $ \accum -> do
        return $! M.insertWith f uploadId (ViewsCacheEntry Nothing len) accum

    -- Signal to the daemon that at least an entry has been added to the cache.
    _ <- liftIO $ signal `tryPutMVar` ()
    return ()
  where
    f _ (ViewsCacheEntry views bw) = ViewsCacheEntry views (bw + len)

-- | Returns the cache entry for the given upload. 'Nothing' if the database is
-- up to date.
getCacheEntry :: UploadId -> Handler (Maybe ViewsCacheEntry)
getCacheEntry uploadId = do
    app <- getYesod
    let ViewsCache cache _ = viewsCache app
    accum <- liftIO $ readMVar cache
    return $! uploadId `M.lookup` accum

-- | Wraps the original ByteString so each transmitted chunk size is commited
-- to the upload's total amount of transferred bytes using the 'ViewsCache'.
-- Updates the upload's last view once the stream has been fully streamed.
getTrackedBS :: UploadId -> L.ByteString -> Handler L.ByteString
getTrackedBS uploadId bs = do
    app <- getYesod
    let bwTracker = addBandwidth app uploadId
        finalizer = incrementViewCount app uploadId
    liftIO $ trackedBS bwTracker finalizer bs

-- | Wraps a 'L.ByteString' so each generated 'L.Chunk' size is commited to the
-- given action. Executes the finalizer when the full 'L.ByteString' has been
-- consumed.
trackedBS :: (Word64 -> IO ()) -> IO () -> L.ByteString -> IO L.ByteString
trackedBS bwTracker finalizer =
    lazyGo . L.toChunks
  where
    lazyGo = unsafeInterleaveIO . go

    go []     = finalizer >> return L.Empty
    go (b:bs) = do
        bwTracker $ word64 $ S.length b
        bs' <- lazyGo bs
        return $! L.Chunk b bs'

-- | Accumulates the views count and the last view for each file to batch update
-- the database every few seconds (saves a lot of disk accesses).
viewsCacheDaemon :: App -> IO ()
viewsCacheDaemon app = do
    let ViewsCache cache signal = viewsCache app
    forever $ do
        -- Waits for at least a file view
        takeMVar signal

        -- Resets the cache.
        oldCache <- cache `swapMVar` M.empty

        -- Inserts the old cache in the database.
        runDBIO app $ do
            forM_ (M.assocs oldCache) $ \(uploadId, entry) ->
                case vbeViews entry of
                    Just (c, t) -> do
                        -- Adds the new views count and computes the new score.
                        mUpload <- get uploadId
                        whenJust mUpload $ \upload ->
                            let views' = (uploadViews upload) + c
                                score' = score (uploadCreated upload) views'
                            in update uploadId [
                                  UploadScore =. score', UploadViews =. views'
                                , UploadViewed =. t
                                , UploadBandwidth +=. vbeBandwidth entry
                                ]
                    Nothing -> update uploadId [
                              UploadBandwidth +=. vbeBandwidth entry
                            ]

        threadDelay commitDelay

-- | Forks the views cache daemon on a new thread and returns its 'ThreadId'.
forkViewsCacheDaemon :: App -> IO ThreadId
forkViewsCacheDaemon = forkIO . viewsCacheDaemon
