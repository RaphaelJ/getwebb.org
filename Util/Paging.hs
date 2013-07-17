module Util.Paging (getPageOffset, pagingWidget) where

import Import

import qualified Data.Text as T

-- | Given the number of items per page, returns the number of the page given by
-- the user in the @page@ GET parameter and the select options corresponding to
-- this page.
getPageOffset :: PersistEntity val => Int -> Handler (Int, [SelectOpt val])
getPageOffset itemsPerPage = do
    mPage <- fmap (read . T.unpack) <$> lookupGetParam "page"

    return $ case mPage of
        Just page | page > 1 ->
            (page, [OffsetBy ((page - 1) * itemsPerPage), LimitTo itemsPerPage])
        _                    ->
            (1   , [LimitTo itemsPerPage])

-- | Returns the widget with the links to the previous and the next page
pagingWidget :: Route App -> Int -> Bool -> Widget
pagingWidget route page hasNext = do
    rdr <- getUrlRenderParams <*> pure route
    $(widgetFile "modules/paging")
  where
    pageRte rdr n = rdr [("page", T.pack $ show n)]
