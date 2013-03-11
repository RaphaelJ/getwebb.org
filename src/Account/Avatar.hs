-- | The functions in this modules defines an identicon algorithm which
-- generates random avatar from a string (eg. the user email).
module Account.Avatar (
      avatarSize, spriteFile, tileSize
    , loadSprite, genIdenticon
    ) where

import Prelude
import Control.Applicative
import Control.Monad
import qualified Data.Array as A
import qualified Data.Binary.Get as G
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Digest.Pure.SHA (sha1, bytestringDigest)
import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath ((</>), (<.>))

import qualified Vision.Image as I
import qualified Vision.Primitive as I
import Yesod

import Account.Foundation

avatarSize, tileSize :: Int
avatarSize = 60

tileSize = 20

spriteFile :: FilePath
spriteFile = "Account" </> "avatar_sprite" <.> "png"

-- | Loads the image file which contains the different tiles and generates a
-- vector of these tiles. Tiles must be arranged in horizontal order.
loadSprite :: IO (A.Array Int I.GreyImage)
loadSprite = do
    img <- I.load spriteFile
    let I.Size w _ = I.getSize img
        n = w `quot` tileSize
        tiles = [ I.crop img (I.Rect x 0 tileSize tileSize)
            | x <- [0,tileSize..n-1] ]
    return $! A.listArray (0, n-1) tiles

-- | Generates a deterministic avatar using an user identifier.
genIdenticon :: Text -> GHandler Account master I.RGBImage
genIdenticon str = do
    sprite  <- acAvatarSprite <$> getYesodSub
    let (color, tilesMap) = G.runGet (getVals sprite) hash
    return $ I.fromFunction (I.Size avatarSize avatarSize) $ \(I.Point x y) ->
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
    nRegions = nSide * nSide

    getVals sprite = do
        -- Uses the first 3 bytes to get a random color.
        color <- I.RGBPixel <$> G.getWord8 <*> G.getWord8 <*> G.getWord8

        -- Uses a byte to choose a random tile for each region.
        let nTiles = 1 + (snd $ A.bounds sprite)
        regions <- replicateM nRegions $ do
            w <- G.getWord8
            return $! sprite A.! (int w `rem` nTiles)

        let maxIdx = nSide - 1
            arr = A.listArray ((0, 0), (maxIdx, maxIdx)) regions

        return (color, arr)

    colorize color 255 = color
    colorize _     _   = I.RGBPixel 0 0 0

avatarPath :: YesodAccount master =>
              AccountUser master -> GHandler sub master FilePath
avatarPath app email = 
    app <- getYesod
    
    let hash = T.pack $ showDigest $ sha1 $ C.pack $ T.unpack email
    avatarDir app </> hashDir' hash

int :: Integral a => a -> Int
int = fromIntegral
