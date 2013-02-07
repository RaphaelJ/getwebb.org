-- | This module defines functions to compute and process Hmacs.
module Utils.Hmac (
      Hmac {- From Model.hs -}
    , computeHmac, splitHmacs, joinHmacs, toBase62
    ) where

import Import

import Data.Array.Unboxed (UArray, listArray, (!))
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Digest.Pure.SHA (hmacSha1, integerDigest)
import Data.Digits (digits)
import qualified Data.Text as T

import Database.Persist.Store (PersistValue (..))

-- | Returns the first eight base 62 encoded digits of the key hmac.
computeHmac :: App -> Key val -> Hmac
computeHmac app idKey =
    let key = encryptKey app
        PersistInt64 idInt = unKey idKey
        hmac = integerDigest $ hmacSha1 key $ C.pack $ show idInt
    in T.pack $ take 8 $ toBase62 $ hmac

-- | Returns a list of hmacs from a list of url string of hmacs separated by
-- commas.
splitHmacs :: Text -> [Text]
splitHmacs = T.split (== ',')

-- | Returns an url string from a list of hmacs.
joinHmacs :: [Text] -> Text
joinHmacs = T.intercalate ","

-- | Encodes an integer in base 62 (using letters and numbers).
toBase62 :: Integer -> String
toBase62 i =
    map (digitToChar !) $ digits 62 i
  where
    digitToChar :: UArray Integer Char
    digitToChar = listArray (0, 61) $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']
