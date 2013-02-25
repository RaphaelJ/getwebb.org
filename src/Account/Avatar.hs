-- | The functions in this modules defines an identicon algorithm which
-- generates random avatar from a string (eg. the user email).
module Account.Avatar (
      spriteFile, tileSize, avatarSize
    , loadSprite, genIdenticon
    ) where

import Prelude
import Control.Applicative
import Control.Monad
import qualified Data.Array as A
import qualified Data.Binary.Get as G
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Digest.Pure.SHA (sha1, showDigest)
import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath ((</>), (<.>))

import qualified Vision.Image as I
import qualified Vision.Primitive as I
import Yesod

import Account.Foundation

spriteFile :: FilePath
spriteFile = "Account" </> "avatar_sprite" <.> "png"

tileSize, avatarSize :: Int
tileSize = 20

avatarSize = 60

-- | Loads the image file which contains the different tiles and generates a
-- vector of these tiles. Tiles must be arranged in horizontal order.
loadSprite :: IO (A.Array Int I.GreyImage)
loadSprite = do
    img <- I.load spriteFile
    let Size w _ = I.getSize img
        n = w `quot` tileSize
        tiles = [ I.crop img (I.Rect x 0 tileSize tileSize)
            | x <- [0,tileSize..n-1] ]
    return $! A.arrayList (0, n-1) tiles

-- | Generates a deterministic avatar using .
genIdenticon :: Text -> GHandler Account master I.RGBImage
genIdenticon str = do
    sprite  <- acAvatarSprite <$> getYesodSub
    (color, tilesMap) <- G.runGet (getVals sprite) hash
    return $ I.fromFunction $ \(I.Point x y) ->
        let (xQuot, xRem) = x `quotRem` tileSize
            (yQuot, yRem) = y `quotRem` tileSize
            tile = tilesMap A.! (xQuot, yQuot)
            greyValue = tile `I.getPixel` I.Point xRem yRem
        in colorize color greyValue
  where
    -- Generates an infinite hash by repeating the application of the SHA1
    -- function.
    hash = C.concat $ tail $ iterate (bytestringDigest . sha1)
                                     (C.pack $ T.unpack str)

    -- Number of tiles on each side of the generated avatar.
    nSide = avatarSize `quot` tileSize
    nRegions = nSide^2

    getVals srite = do
        -- Uses the first 3 bytes to get a random color.
        color <- I.RGBPixel <$> G.getWord8 <*> G.getWord8 <*> G.getWord8

        -- Uses a byte to choose a random tile for each region.
        let nTiles = 1 + (snd $ A.bounds sprite)
        regions <- replicateM nRegions $ do
            w <- G.getWord8
            sprite A.! (w `rem` nTiles)

        let maxIdx = nSide - 1
            arr = A.arrayList ((0, 0), (maxIdx, maxIdx)) regions

        return (color, arr)

    colorize color (I.GreyPixel 255) = rgb
    colorize _     _                 = I.RGBPixel 0 0 0
