{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Util.Hmac.Type where

import Prelude

import Control.Arrow (first)
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as T

import Database.Persist.Sql (PersistFieldSql)
import Yesod

-- | HMACs are unique identifiers used for files, uploads, comments and archive
-- files.
newtype Hmac = Hmac Text
    deriving (Eq, IsString, PersistField, PersistFieldSql, PathPiece, ToJSON)

instance Show Hmac where
    show (Hmac txt) = show txt

instance Read Hmac where
    readsPrec n str = map (first Hmac) (readsPrec n str)
    readList str = map (first (map Hmac)) (readList str)

newtype Hmacs = Hmacs [Hmac]
    deriving (Eq)

-- GHC 7.6 crashes when this instance is used in routes.
instance PathPiece Hmacs where
    fromPathPiece = fmap Hmacs . mapM fromPathPiece . T.split (== ',')

    toPathPiece (Hmacs hmacs) = T.intercalate "," (map toPathPiece hmacs)
