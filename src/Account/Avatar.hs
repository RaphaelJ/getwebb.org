{-# LANGUAGE OverloadedStrings #-}
-- | The functions in this modules defines are used to generate user avatars,
-- to resize uploaded images, to add and remove avatars in the database and to
-- get paths and routes to avatar files.
module Account.Avatar (
      AvatarImage, ImageHash (..), aiImage, aiGenerated, aiHash
    , avatarSize, spriteFile, tileSize, loadSprite
    , genIdenticon, genAvatar
    , newAvatar, removeAvatar, getAvatarId, getAvatar, getAvatarFile
    , avatarPath, avatarRoute, hashImage
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

import qualified Vision.Image as I
import qualified Vision.Primitive as I
import Yesod

import Account.Foundation
import Util.HashDir (hashDir, hashDir')

-- | Abstract data type used to enforce images to be resized before being
-- inserted in the database.
data AvatarImage = AvatarImage {
      aiImage :: !I.RGBAImage, aiGenerated :: !Bool, aiHash :: !ImageHash
    }

newtype ImageHash = ImageHash Text

avatarSize, tileSize :: Int
avatarSize = 4 * tileSize -- Must be a multiple of 2 * tileSize

-- | Size of the tiles in 'spriteFile'.
tileSize = 20

spriteFile :: FilePath
spriteFile = "Account" </> "avatar_sprite" <.> "png"

-- | Loads the image file which contains the different tiles and generates a
-- vector of these tiles. Tiles must be arranged in horizontal order.
loadSprite :: IO Sprite
loadSprite = do
    img <- I.load spriteFile
    let I.Size w _ = I.getSize img
        n = w `quot` tileSize
        tiles = [ I.crop img (I.Rect x 0 tileSize tileSize)
            | let maxX = tileSize * (n - 1)
            , x <- [0,tileSize..maxX] ]
    return $! Sprite $! A.listArray (0, n-1) tiles

-- | Generates a deterministic avatar using an user identifier.
genIdenticon :: Sprite -> Text -> AvatarImage
genIdenticon (Sprite sprite) str =
    let (color, tiles) = G.runGet getVals hash
        regionsArr = regions color tiles

        -- Combines regions into a single image.
        img = I.fromFunction (I.Size avatarSize avatarSize) $ \(I.Point x y) ->
            let (xQuot, xRem) = x `quotRem` regionSide
                (yQuot, yRem) = y `quotRem` regionSide
                region = regionsArr A.! (xQuot + yQuot * 2)
            in region `I.getPixel` I.Point xRem yRem
        img' = I.force img
    in AvatarImage img' True (hashImage img')
  where
    -- Generates an infinite hash by repeating the application of the SHA1
    -- function.
    hash = C.concat $ tail $ iterate (bytestringDigest . sha1)
                                     (C.pack $ T.unpack str)

    -- The avatar is divided in 4 symmetrical regions. Each region is randomly
    -- generated from a square number of randomly chosen tiles.
    regions :: (I.GreyPixel -> I.RGBAPixel) -> A.Array Int I.GreyImage
            -> A.Array Int I.RGBAImage
    regions color tiles =
        let region1 = I.force $ I.fromFunction regionSize $ \(I.Point x y) ->
                let (xQuot, xRem) = x `quotRem` tileSize
                    (yQuot, yRem) = y `quotRem` tileSize
                    tile = tiles A.! (xQuot + yQuot * nTilesSide)
                in color $ tile `I.getPixel` I.Point xRem yRem
            region2 = I.force $ I.horizontalFlip region1
            region3 = I.verticalFlip region1
            region4 = I.verticalFlip region2
        in A.listArray (0, 3) [region1, region2, region3, region4]

    regionSide = avatarSize `quot` 2
    regionSize = I.Size regionSide regionSide

    -- Each region is composed of a square number of tiles.
    nTilesSide = regionSide `quot` tileSize
    nTiles = nTilesSide * nTilesSide

    -- Takes a random color and a set of random tiles from a bytestring.
    getVals = do
        -- Uses the first 3 bytes to get a random color.
        color <- I.RGBAPixel <$> G.getWord8 <*> G.getWord8 <*> G.getWord8

        -- Uses a byte to choose random tiles.
        let availTiles = 1 + snd (A.bounds sprite)
        tiles <- replicateM nTiles $ do
            w <- G.getWord8
            return $! sprite A.! (int w `rem` availTiles)

        let arr = A.listArray (0, nTiles - 1) tiles

        return (color, arr)

-- | Resizes and adapts an uploaded image to be used as an avatar.
genAvatar :: I.RGBAImage -> AvatarImage
genAvatar img =
    let img' = I.force $ I.resize I.Bilinear img (I.Size avatarSize avatarSize)
    in AvatarImage img' False (hashImage img')

-- | Registers and saves a new avatar. Checks if the same avatar is not used by
-- another user.
newAvatar :: YesodAccount parent => AvatarImage -> YesodDB parent AvatarNum
newAvatar (AvatarImage img generated imgHash@(ImageHash hash)) = do
    mFile <- getBy $ UniqueAvatarFileHash hash
    case mFile of
        Just (Entity fileId _) -> do
            update fileId [AvatarFileCount +=. 1]
        Nothing                -> do
            app <- lift $ getYesod
            let path = avatarPath app imgHash
            liftIO $ createDirectoryIfMissing True (takeDirectory path)
            liftIO $ I.save img path
            insert_ $ AvatarFile hash 1
    Key (PersistInt64 avatarId) <- insert $ Avatar generated hash
    return avatarId

-- | Removes a user\'s avatar from the database and from the filesystem if it
-- is not shared by other users.
removeAvatar :: YesodAccount parent => AvatarId -> Avatar -> YesodDB parent ()
removeAvatar avatarId avatar = do
    delete avatarId

    let hash = ImageHash $ avatarHash avatar
    Just (Entity fileId file) <- getAvatarFile avatar

    if avatarFileCount file <= 1
        then do
            delete fileId
            app <- lift getYesod
            liftIO $ removeFile $ avatarPath app hash
        else update fileId [AvatarFileCount -=. 1]

getAvatarId :: (MonadHandler m, YesodAccount (HandlerSite m)) =>
               AccountUser (HandlerSite m) -> m AvatarId
getAvatarId user = do
    app <- getYesod
    return $ Key $ PersistInt64 $ accountAvatarId app user

getAvatar :: YesodAccount parent =>
             AccountUser parent -> YesodDB parent (Maybe Avatar)
getAvatar user = lift (getAvatarId user) >>= get

getAvatarFile :: YesodAccount parent =>
                 Avatar -> YesodDB parent (Maybe (Entity AvatarFile))
getAvatarFile avatar = getBy $ UniqueAvatarFileHash $ avatarHash avatar

avatarPath :: YesodAccount parent => parent -> ImageHash -> FilePath
avatarPath app (ImageHash hash) = avatarsDir app </> hashDir hash <.> "png"

avatarRoute :: YesodAccount parent => parent -> Avatar -> Route parent
avatarRoute app avatar =
    let path = hashDir' $ avatarHash avatar
        file = init path ++ [last path `T.append` ".png"]
    in avatarsDirRoute app file

-- | Returns the SHA1 of the pixels values of the image.
hashImage :: I.RGBAImage -> ImageHash
hashImage =
    ImageHash . T.pack . showDigest . sha1 . L.pack . concat 
              . map I.pixToValues . I.toList

int :: Integral a => a -> Int
int = fromIntegral
