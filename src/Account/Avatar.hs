{-# LANGUAGE OverloadedStrings #-}
-- | The functions in this modules defines an identicon algorithm which
-- generates random avatar from a string (eg. the user email).
module Account.Avatar (
      AvatarImage, avatarSize, spriteFile, tileSize
    , loadSprite, genIdenticon
    , newAvatar, removeAvatar, getUserAvatar, avatarPath, avatarRoute
    , hashImage
    ) where

import Prelude
import Control.Applicative
import Control.Monad
import qualified Data.Array as A
import qualified Data.Binary.Get as G
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Digest.Pure.SHA (sha1, bytestringDigest, showDigest)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (createDirectoryIfMissing, removeFile)
import System.FilePath ((</>), (<.>), takeDirectory)

import Database.Persist.Store (PersistValue (..))
import qualified Vision.Image as I
import qualified Vision.Primitive as I
import Yesod

import Account.Foundation
import Util.Path (hashDir, hashDir')

-- | Abstract data type used to enforce images to be resized before database
-- insertion.
data AvatarImage = AvatarImage { aiImage :: I.RGBAImage, aiGenerated :: !Bool }

avatarSize, tileSize :: Int
avatarSize = 4 * tileSize -- Must be a multiple of 2 * tileSize

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
            | let maxX = tileSize * (n - 1)
            , x <- [0,tileSize..maxX] ]
    return $! A.listArray (0, n-1) tiles

-- | Generates a deterministic avatar using an user identifier.
genIdenticon :: Text -> GHandler Account master AvatarImage
genIdenticon str = do
    sprite  <- acAvatarSprite <$> getYesodSub
    let (color, tiles) = G.runGet (getVals sprite) hash
        regionsArr = regions color tiles

    -- Combines regions in a single image.
    let img = I.fromFunction (I.Size avatarSize avatarSize) $ \(I.Point x y) ->
        let (xQuot, xRem) = x `quotRem` regionSide
            (yQuot, yRem) = y `quotRem` regionSide
            region = regionsArr A.! (xQuot + yQuot * 2)
        in region `I.getPixel` I.Point xRem yRem

    return $ AvatarImage img True
  where
    -- Generates an infinite hash by repeating the application of the SHA1
    -- function.
    hash = C.concat $ tail $ iterate (bytestringDigest . sha1)
                                     (C.pack $ T.unpack str)

    -- The avatar is divided in 4 symmetrical regions.
    regions :: (I.GreyPixel -> I.RGBAPixel) -> A.Array Int I.GreyImage
            -> A.Array Int I.RGBAImage
    regions color tiles =
        let region1 = I.force $ I.fromFunction regionSize $ \(I.Point x y) ->
                let (xQuot, xRem) = x `quotRem` tileSize
                    (yQuot, yRem) = y `quotRem` tileSize
                    tile = tiles A.! (xQuot + yQuot * nTilesSide)
                in color $ tile `I.getPixel` I.Point xRem yRem
            region2 = I.force $ I.horizontalFlip region1
            region3 = I.verticalFlip   region1
            region4 = I.verticalFlip   region2
        in A.listArray (0, 3) [region1, region2, region3, region4]

    regionSide = avatarSize `quot` 2
    regionSize = I.Size regionSide regionSide

    -- Each region is composed of different tiles.
    nTilesSide = regionSide `quot` tileSize
    nTiles = nTilesSide * nTilesSide

    -- Takes a random color and a set of random tiles from a bytestring.
    getVals sprite = do
        -- Uses the first 3 bytes to get a random color.
        color <- I.RGBAPixel <$> G.getWord8 <*> G.getWord8 <*> G.getWord8

        -- Uses a byte to choose random tiles.
        let availTiles = 1 + snd (A.bounds sprite)
        tiles <- replicateM nTiles $ do
            w <- G.getWord8
            return $! sprite A.! (int w `rem` availTiles)

        let arr = A.listArray (0, nTiles - 1) tiles

        return (color, arr)

-- | Registers and saves a new avatar.
newAvatar :: (YesodAccount master, PersistEntityBackend Avatar
              ~ PersistMonadBackend (YesodDB sub master)
             , PersistQuery (YesodDB sub master)
             , PersistUnique (YesodDB sub master)) =>
             I.RGBAImage -> Bool -> YesodDB sub master AvatarId
newAvatar img generated = do
    let hash = hashImage img

    mFile <- getBy $ UniqueAvatarFileHash hash
    case mFile of
        Just (Entity fileId _) -> do
            update fileId [AvatarFileCount +=. 1]
        Nothing                -> do
            app <- lift $ getYesod
            let path = avatarPath app hash
            liftIO $ createDirectoryIfMissing True (takeDirectory path)
            liftIO $ I.save img path
            insert_ $ AvatarFile hash 1
    insert $ Avatar generated hash

-- | Removes a user\'s avatar from the database and from the filesystem if it
-- is not used by other users.
removeAvatar :: (YesodAccount master, PersistEntityBackend Avatar
                 ~ PersistMonadBackend (YesodDB sub master)
                , PersistQuery (YesodDB sub master)
                , PersistUnique (YesodDB sub master)
                , MonadLift (GHandler Account master) (YesodDB sub master)) =>
                AccountUser master -> YesodDB sub master ()
removeAvatar user = do
    Just avatar <- getUserAvatar user
    let hash = avatarHash avatar
    Just (Entity fileId file) <- getBy $ UniqueAvatarFileHash hash

    if avatarFileCount file <= 1
        then do
            delete fileId
            app <- lift getYesod
            liftIO $ removeFile $ avatarPath app hash
        else update fileId [AvatarFileCount -=. 1]

getUserAvatar :: (YesodAccount master, PersistEntityBackend Avatar
                  ~ PersistMonadBackend (YesodDB sub master)
                 , PersistStore (YesodDB sub master)
                 , MonadLift (GHandler sub master) (YesodDB sub master)) =>
                 AccountUser master -> YesodDB sub master (Maybe Avatar)
getUserAvatar user = do
    app <- lift getYesod
    get $ Key $ PersistInt64 $ accountAvatarId app user

avatarPath :: YesodAccount master => master -> Text -> FilePath
avatarPath app hash = avatarsDir app </> hashDir hash <.> "png"

avatarRoute :: YesodAccount master => master -> Avatar -> Route master
avatarRoute app avatar =
    let path = hashDir' $ avatarHash avatar
        file = init path ++ [last path `T.append` ".png"]
    in avatarsDirRoute app file

-- | Returns the SHA1 of the pixels values of the image.
hashImage :: I.RGBAImage -> Text
hashImage =
    T.pack . showDigest . sha1 . L.pack . concat . map I.pixToValues . I.toList

int :: Integral a => a -> Int
int = fromIntegral
