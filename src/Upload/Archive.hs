{-# LANGUAGE OverloadedStrings #-}
-- | Recognises zip archives and adds their content to the database.
module Upload.Archive (processArchive, extensions)
    where

import Import

import qualified Data.Set as S
import System.FilePath

import qualified Data.ByteString.Lazy as B
import qualified Codec.Archive.Zip as Z
import Control.DeepSeq (NFData (..), force, deepseq)

-- | Instance which force the evaluation of the archive index so the exceptions
-- can be catched in the IO monad.
instance NFData Z.Entry where
    rnf e = Z.eRelativePath e      `deepseq`
            Z.eUncompressedSize e  `deepseq`
            ()

-- | Files extensions which are supported by the zip-archive package.
extensions :: S.Set Text
extensions = S.fromDistinctAscList [".zip"]

processArchive :: Text -> FileId -> FilePath -> Handler Bool
processArchive ext fileId dir = do
    if not (ext `S.member` extensions)
        then return False
        else do
            let path = 
            mEntries <- liftIO $ C.catch ((force . Just) <$> readArchiveFiles)
                                         (const $ return Nothing)

            case mEntries of
                Just entries -> runDB $ do
                    -- Appends the files of the archive to the database.
                    forM_ entries $ \e ->
                        let path = Z.eRelativePath e
                            size = Z.eUncompressedSize e
                        in insert $ FileArchive fileId path size
                    return True
                Nothing  -> return False
  where
    -- Try to read the archive index of a zip file.
    readArchiveFiles =
        let path = dir </> "original"
        in B.readFile path >>= return . Z.zEntries . Z.toArchive