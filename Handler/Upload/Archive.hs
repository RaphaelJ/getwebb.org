{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
-- | Recognises zip archives and adds their content to the database.
module Handler.Upload.Archive (
      ArchiveTree (..), extensions, processArchive, archiveTree, treeToHtml
    ) where

import Import

import qualified Control.Exception as E
import Control.Monad
import qualified Data.ByteString.Lazy as B
import Data.List (foldl', sortBy)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import System.FilePath (splitDirectories, hasTrailingPathSeparator)

import qualified Codec.Archive.Zip as Z

import qualified JobsDaemon.Compression as C
import Util.Hmac (Hmac, newHmac)
import Util.Pretty (PrettyFileSize (..), wrappedText)

-- | Represents a hierarchy of files within an archive.
data ArchiveTree = NodeDirectory (M.Map FilePath ArchiveTree)
                 | NodeFile Hmac Word64

-- | Files extensions which are supported by the zip-archive package.
extensions :: S.Set Text
extensions = S.fromDistinctAscList [".apk", ".jar", ".zip"]

-- | Try to read the content of a zip archive.
processArchive :: FilePath -> Text -> FileId -> Handler Bool
processArchive path ext fileId | not (ext `S.member` extensions) = return False
                               | otherwise = do
    eEntries <- liftIO $ E.try (readArchiveFiles >>= E.evaluate)

    case eEntries of
        Right entries -> do
            app <- getYesod
            runDB $ do
                update fileId [FileType =. Archive]

                -- Appends the files of the archive to the database.
                forM_ entries $ \e -> do
                    let ePath = Z.eRelativePath e
                        ePathText = T.pack ePath

                    exists <- getBy $ UniqueArchiveFile fileId ePathText

                    case exists of
                        Just _ -> return ()
                        Nothing -> do
                            (key, hmac) <- newHmac HmacArchiveFile

                            let eSize | hasTrailingPathSeparator ePath =
                                        Nothing
                                      | otherwise =
                                        let size = Z.eUncompressedSize e
                                        in Just $ word64 size

                            insertKey key $ ArchiveFile hmac fileId ePathText
                                                        eSize

            _ <- liftIO $ C.putFile app fileId []
            return True
        Left (_ :: E.SomeException) -> return False
  where
    -- Try to read the archive index of a zip file.
    readArchiveFiles =
        B.readFile path >>= return . Z.zEntries . Z.toArchive

-- | Transforms the list of files in a hierarchy.
archiveTree :: [ArchiveFile] -> ArchiveTree
archiveTree =
    foldl' insertFile emptyDir . map preprocess
  where
    preprocess (ArchiveFile hmac _ path size) =
        (hmac, splitDirectories $ T.unpack path, size)

    insertFile (NodeDirectory files) (_, [e], Nothing) =
        -- Inserts a directory
        let files' = M.insertWith (\_ old -> old) e emptyDir files
        in NodeDirectory files'
    insertFile (NodeDirectory files) (hmac, [e], Just size) =
        -- Inserts a file
        let files' = M.insert e (NodeFile hmac size) files
        in NodeDirectory files'
    insertFile (NodeDirectory files) (hmac, (e:es), size) =
        -- Inserts recursively
        let exists old = insertFile old (hmac, es, size)
            new = insertFile emptyDir (hmac, es, size)
            files' = M.insertWith (\_ old -> exists old) e new files
        in NodeDirectory files'
    insertFile tree _ = tree -- File already in the tree, ignore

    emptyDir = NodeDirectory M.empty

-- | Renders the archive tree with HTML tags using the given function to render
-- URLs to download files separately.
treeToHtml :: (Hmac -> Text) -> ArchiveTree -> Html
treeToHtml rdrUrl ~(NodeDirectory tree) = [shamlet|
    <ul .last>
        ^{go $ sortFiles tree}
    |]
  where
    go []  = mempty
    go ((name, item):is) = -- Last item of a level
        let nodeClass = if null is then "last" else "" :: Text
            name' = wrappedText (T.pack name) 30
        in case item of
            NodeDirectory files -> [shamlet|
                <li class=#{nodeClass}>
                    <span .directory title=#{name}>#{name'}
                    <ul>
                        ^{go (sortFiles files)}
                |]
            NodeFile hmac size ->
                let url = rdrUrl hmac
                in [shamlet|
                    <li class=#{nodeClass}>
                        <a .file href=#{url} download=#{name} title=#{name}>
                            #{name'}
                        <span .size>#{PrettyFileSize size}
                    |]
           `mappend` go is

    -- Extracts and sorts a single level of mapped files. Directories first.
    sortFiles = sortBy cmpNodes . M.toList

    cmpNodes (_, NodeDirectory {}) (_, NodeFile {}) = LT
    cmpNodes (_, NodeFile {}) (_, NodeDirectory {}) = GT
    cmpNodes (name1, _) (name2, _) = name1 `compare` name2
