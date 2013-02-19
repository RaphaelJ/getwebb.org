{-# LANGUAGE OverloadedStrings #-}
-- | Defines ToJson instances to transform models and types in json objects.
module Util.Json (CommentUser) where

import Import

import Data.Aeson (ToJSON (..), (.=), object)

import Util.Date (Rfc822Date)

instance ToJSON User where
    toJSON u = object [
          "name"          .= userName u
        ]

newtype CommentUser = CommentUser Comment User

instance ToJSON CommentUser where
    toJSON (CommentUser c u) = object [
          "id"          .= commentHmac c
        , "user"        .= toJson u
        , "message"     .= commentMessage c
        , "created_at"  .= Rfc822Date $ commentCreated c
        , "score"       .= commentScore
        , "upvotes"     .= commentUpvotes c
        , "downvotes"   .= commentDownvotes c
        ]

instance ToJSON Rfc822Date where
    toJSON = toJSON . show