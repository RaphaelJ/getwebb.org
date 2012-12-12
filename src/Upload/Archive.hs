{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Recognises zip archives and adds their content to the database.
module Upload.Archive (processArchive, extensions)
    where

import Import

import qualified Control.Exception as C
import Control.Monad
import qualified Data.Set as S
import System.FilePath

import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T
import qualified Codec.Archive.Zip as Z
import Control.DeepSeq (NFData (..), force, deepseq)

-- | Files extensions which are supported by the zip-archive package.
extensions :: S.Set Text
extensions = S.fromDistinctAscList [".zip"]

-- | Try to read the content of a zip archive.
processArchive :: FilePath -> FilePath -> Text -> FileId -> Handler Bool
processArchive dir path ext fileId = do
    if not (ext `S.member` extensions)
        then return False
        else do
            eEntries <- liftIO $ C.try (readArchiveFiles >>= C.evaluate)

            case eEntries of
                Right entries -> runDB do
                    -- Appends the files of the archive to the database.
                    forM_ entries $ \e ->
                        let path = T.pack $ Z.eRelativePath e
                            size = fromIntegral $ Z.eUncompressedSize e
                        in insert $ ArchiveFile fileId path size
                    return True
                Left (_ :: C.SomeException) -> return False
  where
    -- Try to read the archive index of a zip file.
    readArchiveFiles =
        B.readFile path >>= return . Z.zEntries . Z.toArchive
