{-# LANGUAGE OverloadedStrings #-}
module Handler.Comment (maxCommentLength, getCommentR, commentForm)
    where

import Import

import qualified Data.Text as T
import Text.Printf

-- | Maximum length of a comment in characters.
maxCommentLength :: Int
maxCommentLength = 400

-- | Returns the first 50 comments of a file.
getCommentR :: Hmac -> Handler ()
getCommentR hmac = do
    return ()

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
        | T.null msg = Left $ T.pack "Your message can't be empty."
        | T.length msg > maxCommentLength =
            Left $ T.pack $ printf "Your message can't exceed %d characters."
                                   maxCommentLength
        | otherwise = Right area