-- | Defines a few utilities to create paths based on file\'s hashes.
module Util.HashDir (hashDir, hashDir') where

import Prelude

import Data.List
import qualified Data.Text as T
import System.FilePath

-- | Splits the hash of the file in four parts and constucts a four levels
-- directory path.
hashDir :: T.Text -> FilePath
hashDir = foldl' (</>) "" . map T.unpack . hashDir'

-- | Splits the hash of the file in four parts.
hashDir' :: T.Text -> [T.Text]
hashDir' hash =
    let (p1, hash') = T.splitAt 2 hash
        (p2, hash'') = T.splitAt 2 hash'
        (p3, p4) = T.splitAt 2 hash''
    in [p1, p2, p3, p4]
