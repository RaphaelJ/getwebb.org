{-# LANGUAGE OverloadedStrings #-}
module Handler.Upload where

import Import

-- Uploads files to the server. Returns a Json object which contains the
-- id of the upload or the error.
postUploadR :: Handler RepJson
postUploadR = do
    undefined