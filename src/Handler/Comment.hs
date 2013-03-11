{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Handler.Comment (
      maxNComments, defaultNComments, maxCommentLength
    , getCommentR, postCommentR, putCommentUpR, putCommentDownR
    , retrieveComments, commentForm, score
    ) where

import Import

import Control.Monad
import Data.Maybe
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)
import Text.Printf

import Network.HTTP.Types.Status (created201, badRequest400)

import Account (requireAuth)
import Util.Hmac (newHmac)
import Util.Json ()

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
getCommentR :: Hmac -> Handler RepJson
getCommentR hmac = do
    mNComments <- lookupGetParam "n"
    mMaxScore  <- lookupGetParam "max_score"
    let nComments = maybe defaultNComments 
                          (max 0 . min maxNComments . read . T.unpack)
                          mNComments
        maxScore  = (max 0 . read . T.unpack) <$> mMaxScore

    comments <- runDB $ do
        Entity uploadId _ <- getBy404 $ UniqueUploadHmac hmac
        retrieveComments uploadId nComments maxScore

    jsonToRepJson $ array comments

-- | Posts a new comment.
postCommentR :: Hmac -> Handler ()
postCommentR hmac = do
    Entity userId _ <- requireAuth
    ((res, _), _) <- runFormPostNoToken commentForm

    case res of
        FormSuccess (Textarea msg)  -> do
            time <- liftIO $ getCurrentTime
            runDB $ do
                Entity uploadId _ <- getBy404 $ UniqueUploadHmac hmac
                (key, commentHmac) <- newHmac HmacComment
                insertKey key $ Comment commentHmac userId uploadId msg time
                                        (score 0 0) 0 0
            sendResponseStatus created201 ()
        FormFailure errs -> do
            rep <- jsonToRepJson $ array errs
            sendResponseStatus badRequest400 rep
        FormMissing      -> do
            rep <- jsonToRepJson $ array [("Incomplete form." :: Text)]
            sendResponseStatus badRequest400 rep

-- | Votes for a comment.
putCommentUpR :: Hmac -> Handler ()
putCommentUpR = voteComment Upvote

-- | Votes against a comment.
putCommentDownR :: Hmac -> Handler ()
putCommentDownR = voteComment Downvote

-- | Votes for or against a comment.
voteComment :: VoteType -> Hmac -> Handler ()
voteComment voteType hmac = do
    Entity userId _ <- requireAuth
    runDB $ do
        Entity commentId Comment { .. } <- getBy404 $ UniqueCommentHmac hmac

        mExists <- getBy $ UniqueCommentVoteUser commentId userId
        (upvotes, downvotes) <- case mExists of
            Just (Entity voteId vote) | voteType /= commentVoteType vote -> do
                -- Replaces the previous vote.
                update voteId [CommentVoteType =. voteType]
                case voteType of
                    Upvote   -> return (commentUpvotes+1, commentDownvotes-1)
                    Downvote -> return (commentUpvotes-1, commentDownvotes+1)
                                      | otherwise ->
                return (commentUpvotes, commentDownvotes)
            Nothing     -> do
                insert_ $ CommentVote commentId userId voteType
                case voteType of
                    Upvote   -> return (commentUpvotes+1, commentDownvotes)
                    Downvote -> return (commentUpvotes, commentDownvotes+1)

        update commentId [ CommentUpvotes   =. upvotes
                         , CommentDownvotes =. downvotes
                         , CommentScore     =. score upvotes downvotes ]

retrieveComments :: UploadId -> Int -> Maybe Double
                 -> YesodDB sub App [(Comment, User)]
retrieveComments uploadId nComments maxScore = do
    let restrict = maybeToList ((CommentScore <.) <$> maxScore)

    cs <- selectList ((CommentUpload ==. uploadId) : restrict)
                     [Desc CommentScore, LimitTo nComments]
    forM cs $ \(Entity _ c) -> do
        u <- getJust $ commentUser c
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
score upvotes downvotes | n' == 0   = 0
                        | otherwise =
    (p + z2 / (2*n) - z * sqrt ((p * (1 - p) + z2 / (4*n)) / n)) / (1 + z2 / n)
  where
    z = 1.96 -- For a confidence of 0.95
    z2 = z**2
    n' = upvotes + downvotes
    n = double n'
    p = double upvotes / n -- Probability to get an upvote.
