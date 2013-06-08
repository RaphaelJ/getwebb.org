{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Defines 'ToJSON' instances to transform common models and types to json
-- objects.
module Util.Json () where

import Import

import Util.Date (Rfc822Date (..))

instance ToJSON User where
    toJSON u = object [
          "name"          .= userName u
        ]

instance ToJSON (Comment, User) where
    toJSON (c, u) = object [
          "id"          .= commentHmac c
        , "user"        .= u
        , "message"     .= commentMessage c
        , "created_at"  .= Rfc822Date (commentCreated c)
        , "score"       .= commentScore c
        , "upvotes"     .= commentUpvotes c
        , "downvotes"   .= commentDownvotes c
        ]

instance ToJSON Rfc822Date where
    toJSON = toJSON . show
