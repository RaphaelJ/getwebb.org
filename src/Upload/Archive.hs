{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Recognises zip archives and adds their content to the database.
module Upload.Archive (processArchive, extensions)
    where

import Import

import qualified Control.Exception as E
import Control.Monad
import qualified Data.ByteString.Lazy as B
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Word

import qualified Codec.Archive.Zip as Z

import qualified Upload.Compression as C

-- | Files extensions which are supported by the zip-archive package.
extensions :: S.Set Text
extensions = S.fromDistinctAscList [".zip"]

-- | Try to read the content of a zip archive.
processArchive :: FilePath -> Text -> FileId -> Handler Bool
processArchive path ext fileId = do
    if not (ext `S.member` extensions)
        then return False
        else do
            eEntries <- liftIO $! E.try (readArchiveFiles >>= E.evaluate)

            case eEntries of
                Right entries -> do
                    runDB $ do
                        -- Appends the files of the archive to the database.
                        forM_ entries $ \e ->
                            let ePath = T.pack $! Z.eRelativePath e
                                size = word64 $! Z.eUncompressedSize e
                            in insert $! ArchiveFile fileId ePath size

                    app <- getYesod
                    liftIO $ app `C.putFile` fileId
                    return True
                Left (_ :: E.SomeException) -> return False
  where
    -- Try to read the archive index of a zip file.
    readArchiveFiles =
        B.readFile path >>= return . Z.zEntries . Z.toArchive

word64 :: Integral a => a -> Word64
word64 = fromIntegral
