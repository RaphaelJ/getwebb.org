-- | Functions to compute and process HMACs.
-- HMACs identify uniquely a resource like an upload, a comment or a file.
-- HMACs are computed using the SHA1 algorithm associed with the encryption key
-- of the application.
module Util.Hmac (
      module Util.Hmac.Type, UniqueHmacId {- From Model -}, hmacLength
    , newHmac, computeHmac, toBase62
    ) where

import Import

import Data.Array.Unboxed (UArray, listArray, (!))
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Digest.Pure.SHA (hmacSha1, integerDigest)
import Data.Digits (digits)
import Data.Maybe
import qualified Data.Text as T

import Util.Hmac.Type

-- | Number of characters in an HMAC.
hmacLength :: Int
hmacLength = 8

-- | Returns a new unique identifier for a resource.
newHmac :: PersistEntity val =>
           HmacResourceType -> YesodDB App (Key val, Hmac)
newHmac resource = do
    hmacId <- insert $ UniqueHmac "" resource

    -- Concatenates the new ID until the generated HMAC is unique
    let PersistInt64 idInt = unKey hmacId
        idBs = C.pack $ show idInt
        idBsInf = iterate (`C.append` idBs) idBs
    hmac <- untilUnique idBsInf

    update hmacId [UniqueHmacValue =. hmac]

    return (Key $ unKey hmacId, hmac)
  where
    untilUnique ~(x:xs) = do
        hmac <- lift $ computeHmac x
        exists <- isJust <$> getBy (UniqueUniqueHmacValue hmac)
        if exists then untilUnique xs
                  else return hmac

-- | Returns the first eight base 62 encoded digits of the key HMAC, using the
-- encryption key of the application.
computeHmac :: C.ByteString -> Handler Hmac
computeHmac idKey = do
    app <- getYesod
    let key = encryptKey app
        hmac = integerDigest $ hmacSha1 key idKey
    return $! Hmac $ T.pack $ take hmacLength $ toBase62 $ hmac

-- | Encodes an integer in base 62 (using letters and numbers).
toBase62 :: Integer -> String
toBase62 i =
    map (digitToChar !) $ digits 62 i
  where
    digitToChar :: UArray Integer Char
    digitToChar = listArray (0, 61) $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']
