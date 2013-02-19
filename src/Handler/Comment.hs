{-# LANGUAGE OverloadedStrings #-}
module Handler.Comment (
      nComments, maxCommentLength
    , getCommentR
    , score)
    where

import Import

import qualified Data.Text as T
import Text.Printf

import Util.Date (rfc822Date)

-- | Maximum number of comments which will be fetched in one request.
nComments :: Int
nComments = 50

-- | Maximum length of a comment in characters.
maxCommentLength :: Int
maxCommentLength = 400

-- | Returns the first 50 comments of a file.
getCommentR :: Hmac -> Handler RepJSON
getCommentR hmac = do
    mMaxScore <- (max 0 . read) <$> lookupGetParam "max_score"
    let maxScore =  mMaxScore

    comments <- runDB $ do
        _ <- getBy404 $ UploadUniqueHmac hmac
        retrieveComments hmac maxScore

    jsonToRepJson $ array [ object [
              "id"         .= commentHmac c
            , "user"       .= object [
                  "name"        .= userName u
                ]
            , "message"    .= commentMessage c
            , "created_at" .= Rfc822Date $ commentCreated c
            , "score"      .= commentScore
            , "upvotes"    .= commentUpvotes c
            , "downvotes"  .= commentDownvotes c
            ]
        | (c, u) <- comments
        ] 

-- | Posts a new comment
postCommentR :: Hmac -> 
postCommentR hmac = do
    

retrieveComments :: Hmac -> Maybe Double -> YesodDB sub App [(Comment, User)]
retrieveComments hmac mMaxScore = do
    let restrict = maybeToList ((CommentScore <) <$> mMaxScore)

    cs <- selectList ((CommentUploadId ==. hmac) : restrict)
                     [Desc CommentScore, LimitTo nComments]
    forM cs $ \(Entity _ c) -> do
        Entity _ u <- getJust $ commentUserId c
        return (c, u)

-- | Creates a form to post a new comment.
commentForm :: Form Textarea
commentForm =
    renderDivs $ areq (check sizeCheck textareaField) messageSettings Nothing
  where
    messageSettings =
        let name = Just "message"
        in FieldSettings {
              fsLabel = "Message", fsTooltip = Nothing, fsId = name
            , fsName = name
            , fsAttrs = [("placeholder", "Say something about this file.")]
            }

    sizeCheck area@(Textarea msg)
        | T.length msg > maxCommentLength =
            Left $ T.pack $ printf "Your message can't exceed %d characters."
                                   maxCommentLength
        | otherwise = Right area

-- | Computes the Wilson confidence score of comment.
-- See <http://www.evanmiller.org/how-not-to-sort-by-average-rating.html>.
score :: Word64 -> Word64 -> Double
score upvotes downvotes =
    (p + z2 / (2*n) - z * sqrt ((p * (1 - p) + z2 / (4*n)) / n)) / (1 + z2 / n)
  where
    z = 1.96 -- For a confidence of 0.95
    z2 = z**2
    n = double $ upvotes + downvotes
    p = double upvotes / n -- Probability to get an upvote.
