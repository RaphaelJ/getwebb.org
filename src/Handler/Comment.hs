{-# LANGUAGE OverloadedStrings #-}
module Handler.Comment (
      maxNComments, defaultNComments, maxCommentLength
    , getCommentR
    , score)
    where

import Import

import qualified Data.Text as T
import Text.Printf

import Util.Json (CommentUser (..))

-- | Maximum number of comments which will be fetched in one request.
maxNComments :: Int
maxNComments = 200

-- | Default number of comments which will be fetched in one request.
defaultNComments :: Int
defaultNComments = 50

-- | Maximum length of a comment in characters.
maxCommentLength :: Int
maxCommentLength = 400

-- | Returns the comments of a file.
getCommentR :: Hmac -> Handler RepJSON
getCommentR hmac = do
    mNComments <- lookupGetParam "n"
    mMaxScore  <- lookupGetParam "max_score"
    let nComments = fromMaybe defaultNComments (max 0 . min maxNComments . read)
        maxScore  = (max 0 . read) <$> mMaxScore

    comments <- runDB $ do
        _ <- getBy404 $ UploadUniqueHmac hmac
        retrieveComments hmac nComments maxScore

    jsonToRepJson $ array $ map CommentUser comments

-- | Posts a new comment.
postCommentR :: Hmac -> Handler ()
postCommentR hmac = do
    (res, _), _) <- runFormPostNoToken commentForm

    case res of
        FormSuccess msg  -> do
            runDB $ do
                
        FormFailure errs -> 
        FormMissing      -> 

retrieveComments :: Hmac -> Maybe Double -> YesodDB sub App [(Comment, User)]
retrieveComments hmac maxScore = do
    let restrict = maybeToList ((CommentScore <.) <$> maxScore)

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
