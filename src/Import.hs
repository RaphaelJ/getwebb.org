module Import
    ( module Import
    ) where

import           Prelude              as Import hiding (head, init, last,
                                                 readFile, tail, writeFile)
import           Yesod                as Import

import           Control.Applicative  as Import (pure, (<$>), (<*>))
import           Data.Int             as Import
import           Data.Text            as Import (Text)
import           Data.Word            as Import

import           Foundation           as Import
import           Model                as Import
import           Settings             as Import
import           Settings.Development as Import
import           Settings.StaticFiles as Import

#if __GLASGOW_HASKELL__ >= 704
import           Data.Monoid          as Import
                                                 (Monoid (mappend, mempty, mconcat),
                                                 (<>))
#else
import           Data.Monoid          as Import
                                                 (Monoid (mappend, mempty, mconcat))

infixr 5 <>
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
#endif

int :: Integral a => a -> Int
int = fromIntegral
word32 :: Integral a => a -> Word32
word32 = fromIntegral
word64 :: Integral a => a -> Word64
word64 = fromIntegral
