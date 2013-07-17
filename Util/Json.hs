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

instance ToJSON (Entity Comment, Entity User, Maybe VoteType) where
    toJSON (Entity _ c, Entity _ u, mVote) = object $
        case mVote of Just vote -> ("vote" .= voteToText vote) : info
                      Nothing   -> info
      where
        info = [
              "id"          .= commentHmac c
            , "user"        .= u
            , "message"     .= (unTextarea $ commentMessage c)
            , "created_at"  .= Rfc822Date (commentCreated c)
            , "score"       .= commentScore c
            , "upvotes"     .= commentUpvotes c
            , "downvotes"   .= commentDownvotes c
            ]

        voteToText Upvote   = "upvote" :: Text
        voteToText Downvote = "downvote"

instance ToJSON Rfc822Date where
    toJSON = toJSON . show
