{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Handler.Comment (
      maxNComments, defaultNComments, maxCommentLength, minCommentInterval
    , getCommentsR, postCommentsR
    , deleteCommentR, putCommentUpR, putCommentDownR
    , retrieveComments, removeComment, commentForm, score
    ) where

import Import

import Control.Monad
import Data.Maybe
import qualified Data.Text as T
import Data.Time.Clock (NominalDiffTime, addUTCTime, getCurrentTime)
import Text.Printf

import Account (getUser, requireAuth)
import Util.API (
      sendObjectCreated, sendNoContent, sendErrorResponse, sendPermissionDenied
    , tooManyRequests429, withFormSuccess
    )
import Util.Hmac (Hmac, newHmac)
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

-- | Seconds between two comments from the same user.
minCommentInterval :: NominalDiffTime
minCommentInterval = 60

-- Handler ---------------------------------------------------------------------

-- | Returns the comments of an upload in a JSON array.
-- Reads two optional GET values :
-- * @offset@ which indicates the number of comments to skip before returning
--   the first one ;
-- * @n@ which indicates the number of comments to return.
getCommentsR :: Hmac -> Handler Value
getCommentsR hmac = do
    mOffset    <- fmap (max 0 . read . T.unpack) <$> lookupGetParam "offset"
    mNComments <- lookupGetParam "n"
    let nComments = maybe defaultNComments
                          (max 0 . min maxNComments . read . T.unpack)
                          mNComments

    mUser <- getUser
    let mUserId = (entityKey . fst) <$> mUser
    comments <- runDB $ do
        Entity uploadId _ <- getBy404 $ UniqueUploadHmac hmac

        retrieveComments mUserId uploadId mOffset (Just nComments)

    return $ array comments

-- | Posts a new comment. Returns a 201 Created with a JSON object indicating
-- the 'Hmac' of the comment, or a 400/403/404/429 with a JSON array of errors.
postCommentsR :: Hmac -> Handler ()
postCommentsR hmac = do
    (Entity userId _, _) <- requireAuth
    ((res, _), _) <- runFormPost commentForm

    withFormSuccess res $ \msg -> do
        time <- liftIO $ getCurrentTime
        commentHmac <- runDB $ do
            -- Checks if the user has submitted a comment too recently.
            mRecent <- selectFirst [
                      CommentUser    ==. userId
                    , CommentCreated >.  addUTCTime (-minCommentInterval) time
                    ] []
            whenJust mRecent $ \_ ->
                sendErrorResponse tooManyRequests429 [
                      printf "Please wait %s between two comments."
                             (show minCommentInterval) :: String
                    ]

            -- Commits the comment and a first upvote.
            Entity uploadId _ <- getBy404 $ UniqueUploadHmac hmac
            liftIO $ print (uploadId, hmac)

            (key, commentHmac) <- newHmac HmacComment
            insertKey key $ Comment commentHmac userId uploadId msg time
                                    (score 1 0) 1 0
            insert $ CommentVote key userId Upvote
            update userId   [UserCommentsCount   +=. 1]
            update uploadId [UploadCommentsCount +=. 1]
            return commentHmac
        sendObjectCreated commentHmac Nothing

-- | Removes a comment. Returns a 204 No content on success, a 404 Not found if
-- doesn't exists or 403 if the user isn't allowed to remove the comment.
deleteCommentR :: Hmac -> Handler ()
deleteCommentR hmac = do
    (Entity userId _, _) <- requireAuth
    runDB $ do
        entity@(Entity _ comment) <- getBy404 $ UniqueCommentHmac hmac
        when (commentUser comment /= userId)
            sendPermissionDenied

        removeComment userId entity
    sendNoContent

-- | Votes for a comment.
putCommentUpR :: Hmac -> Handler ()
putCommentUpR = voteComment Upvote

-- | Votes against a comment.
putCommentDownR :: Hmac -> Handler ()
putCommentDownR = voteComment Downvote

-- | Votes for or against a comment. Returns a 204 No content on success, a 404
-- Not found if the upload doesn't exist or 403 if the user isn't allowed to
-- vote for a comment.
voteComment :: VoteType -> Hmac -> Handler ()
voteComment voteType hmac = do
    (Entity userId _, _) <- requireAuth
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
    sendNoContent

-- Utilities -------------------------------------------------------------------

-- | Retrieves a set of comments from the database.
-- Can give the offset of the first comment and the number of returned comments.
retrieveComments :: Maybe UserId -> UploadId -> Maybe Int -> Maybe Int
                 -> YesodDB App [(Entity Comment, Entity User, Maybe VoteType)]
retrieveComments mUserId uploadId mOffset mNComments = do
    let opts = Desc CommentScore : catMaybes [
                 OffsetBy <$> mOffset
               , LimitTo  <$> mNComments
               ]

    cs <- selectList [CommentUpload ==. uploadId] opts
    forM cs $ \entity@(Entity commentId comment) -> do
        let authorId = commentUser comment
        author <- Entity authorId <$> getJust authorId
        case mUserId of
            Just userId -> do
                mVote <- selectFirst [ CommentVoteComment ==. commentId
                                     , CommentVoteUser    ==. userId ] []
                return (entity, author, (commentVoteType . entityVal) <$> mVote)
            Nothing     ->
                return (entity, author, Nothing)

-- | Removes a comment from the database and decrements counters.
removeComment :: UserId -> Entity Comment -> YesodDB App ()
removeComment userId (Entity commentId comment) = do
    update userId                  [UserCommentsCount   -=. 1]
    update (commentUpload comment) [UploadCommentsCount -=. 1]
    deleteWhere [CommentVoteComment ==. commentId]
    delete commentId

-- | Creates a form to post a new comment.
commentForm :: Form Textarea
commentForm =
    renderDivs $ areq (check sizeCheck textareaField) messageSettings Nothing
  where
    messageSettings = FieldSettings {
              fsLabel = "Message", fsTooltip = Nothing
            , fsId = Just "comment_message", fsName = Just "message"
            , fsAttrs = [("placeholder", "Say something about this file.")]
            }

    sizeCheck area@(Textarea msg)
        | T.length msg > maxCommentLength =
            Left $ T.pack $ printf "Your message can't exceed %d characters."
                                   maxCommentLength
        | otherwise = Right area

-- | Computes the Wilson confidence score of comment.
-- See <http://www.evanmiller.org/how-not-to-sort-by-average-rating.html>.
score :: Int -> Int -> Double
score upvotes downvotes | n' == 0   = 0 -- Avoid division by 0.
                        | otherwise =
    (p + z2 / (2*n) - z * sqrt ((p * (1 - p) + z2 / (4*n)) / n)) / (1 + z2 / n)
  where
    z = 1.96 -- For a confidence of 0.95
    z2 = z**2
    n' = upvotes + downvotes
    n = double n'
    p = double upvotes / n -- Probability to get an upvote.
