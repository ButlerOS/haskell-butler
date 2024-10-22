-- | This module contains the file system logic.
module Butler.Core.File (
    -- * File system data types
    Entry (..),
    File (name, size),
    Directory (path, parent, childs),
    readRootDirectory,

    -- * Directory API
    newDirectory,
    getRootDir,
    FileName,
    lookupChild,
    baseName,
    refreshDirectory,
    readDirectoryEntries,
    deleteDirectory,

    -- * File API
    createFile,
    readFileBS,
    writeFileBS,
    appendFileBS,
    deleteFile,

    -- * Rename/Move API
    renameEntry,
    moveEntry,

    -- * External API
    FileLoc,
    getFileLoc,
    resolveFileLoc,

    -- * Upload API
    PartialFile,
    createPartialFile,
    appendPartialFile,
    finalizePartialFile,

    -- * Mime API
    ContentType (..),
    fileContentType,
) where

import Butler.Prelude

import Data.ByteString qualified as BS
import Data.Text qualified as Text
import System.Directory (doesPathExist, removeDirectoryRecursive)
import System.Posix (COff (..), fileSize, isRegularFile)
import System.Posix.ByteString (isDirectory, openDirStream)
import System.Posix.Directory.ByteString (readDirStream)
import System.Posix.Files.ByteString (getFileStatus, removeLink, rename)
import System.Posix.Types (FileOffset)

-- | 'Entry' is the base element of a file system.
data Entry
    = File File
    | Directory Directory

instance From Entry FileName where
    from = \case
        File file -> from file.name
        Directory dir -> from dir.path

data File = MkFile
    { name :: RawFilePath
    -- ^ The file name
    , size :: TVar FileOffset
    -- ^ The file size
    }

instance ToJSON File where
    toJSON file = object ["file" .= decodeUtf8 file.name]

data Directory = MkDirectory
    { path :: RawFilePath
    -- ^ The directory path
    , parent :: Maybe Directory
    -- ^ The parent directory, Nothing for the root directory.
    , childs :: TVar [Entry]
    -- ^ The directory content
    , lock :: MVar ()
    -- ^ A lock to prevent concurrent directory read.
    }

instance ToJSON Directory where
    toJSON dir = object ["dir" .= decodeUtf8 (baseName dir.path)]

-- | Return the outer most directory.
getRootDir :: Directory -> Directory
getRootDir dir = maybe dir getRootDir dir.parent

newDirectory :: MonadUnliftIO m => Directory -> FileName -> m (Maybe Directory)
newDirectory _ "" = pure Nothing
newDirectory parent name =
    lookupChild parent name >>= \case
        Just (Directory dir) -> pure (Just dir)
        Just File{} -> pure Nothing
        Nothing -> do
            when ('/' `Text.elem` coerce name) do
                error "NotImplemented: multiple directories creation at once"
            dir <- MkDirectory (mconcat [parent.path, "/", from name]) (Just parent) <$> newTVarIO [] <*> newMVar ()
            ensureDirectory dir
            atomically $ modifyTVar' parent.childs (Directory dir :)
            pure (Just dir)

deleteDirectory :: MonadIO m => Directory -> Directory -> m ()
deleteDirectory parent dir = do
    liftIO do
        whenM (doesPathExist dpath) (removeDirectoryRecursive dpath)
    atomically $ modifyTVar' parent.childs (filter notDir)
  where
    dpath = from $ decodeUtf8 dir.path
    notDir = \case
        Directory d -> d.path /= dir.path
        File{} -> True

baseName :: RawFilePath -> RawFilePath
baseName = BS.takeWhileEnd (/= unsafeFrom (fromEnum '/'))

dirName :: RawFilePath -> RawFilePath
dirName = BS.dropWhileEnd (/= unsafeFrom (fromEnum '/'))

instance ToJSON Entry where
    toJSON = \case
        File f -> object ["file" .= decodeUtf8 f.name]
        Directory d -> object ["dir" .= decodeUtf8 (baseName d.path)]

getFilePath :: Directory -> File -> FilePath
getFilePath dir file = from $ decodeUtf8 (mconcat [dir.path, "/", file.name])

readFileBS :: MonadIO m => Directory -> File -> m ByteString
readFileBS dir file = liftIO $ BS.readFile (getFilePath dir file)

createFile :: MonadIO m => Directory -> FileName -> ByteString -> m File
createFile dir name buf = do
    file <- MkFile (from name) <$> newTVarIO 0
    atomically (addFile dir file)
    writeFileBS dir file buf
    pure file

addFile :: Directory -> File -> STM ()
addFile dir file = modifyTVar' dir.childs (\childs -> File file : filter notFile childs)
  where
    notFile = \case
        File f -> f.name /= file.name
        _ -> True

bufLength :: ByteString -> FileOffset
bufLength = COff . from . BS.length

writeFileBS :: MonadIO m => Directory -> File -> ByteString -> m ()
writeFileBS dir file buf = do
    ensureDirectory dir
    liftIO $ BS.writeFile (getFilePath dir file) buf
    atomically $ writeTVar file.size (bufLength buf)

appendFileBS :: MonadIO m => Directory -> File -> ByteString -> m ()
appendFileBS dir file buf = do
    ensureDirectory dir
    liftIO $ BS.appendFile (getFilePath dir file) buf
    atomically $ modifyTVar' file.size (+ bufLength buf)

deleteFile :: MonadIO m => Directory -> File -> m ()
deleteFile dir file = do
    liftIO $ removeLink (mconcat [dir.path, "/", file.name])
    atomically $ modifyTVar' dir.childs (filter notFile)
  where
    notFile = \case
        Directory{} -> True
        File ofile -> ofile.name /= file.name

{- | Read the root directory content, return Nothing when the file path is not a directory.
You must call this function to get the first directory.
-}
readRootDirectory :: MonadIO m => RawFilePath -> m (Maybe Directory)
readRootDirectory rootDir = do
    eStat <- liftIO $ try $ getFileStatus rootDir
    case eStat of
        Left (_ :: SomeException) -> do
            -- The directory does not exist
            dir <- mkRootDir
            ensureDirectory dir
            pure (Just dir)
        Right stat
            | isDirectory stat -> do
                dir <- mkRootDir
                void $ readDirectory dir
                pure (Just dir)
            | otherwise -> pure Nothing
  where
    mkRootDir = MkDirectory (removeTrailingSlash rootDir) Nothing <$> newTVarIO [] <*> newMVar ()

readDirectoryImpl :: MonadIO m => Directory -> (RawFilePath -> m (Maybe a)) -> m [a]
readDirectoryImpl dir mkEntry = do
    dstream <- liftIO $ openDirStream dir.path
    let getChilds acc =
            liftIO (readDirStream dstream) >>= \case
                cpath
                    | cpath == mempty -> pure acc
                    | cpath `elem` ["", ".", ".."] -> getChilds acc
                    | otherwise ->
                        mkEntry cpath >>= \case
                            Nothing -> getChilds acc
                            Just e -> getChilds (e : acc)
    getChilds []

readDirectory :: MonadIO m => Directory -> m [Entry]
readDirectory dir = do
    entries <- readDirectoryImpl dir \cpath -> readEntry dir (mconcat [dir.path, "/", cpath])
    atomically $ writeTVar dir.childs entries
    pure entries

refreshDirectory :: MonadUnliftIO m => Directory -> m ()
refreshDirectory dir = withMVar dir.lock \() -> do
    curEntries <- readTVarIO dir.childs
    newEntries <- readDirectoryImpl dir \cpath ->
        case isKnownEntry cpath curEntries of
            Just e -> pure $ Just e
            Nothing -> readEntry dir (mconcat [dir.path, "/", cpath])
    atomically $ writeTVar dir.childs newEntries
  where
    isKnownEntry :: RawFilePath -> [Entry] -> Maybe Entry
    isKnownEntry _ [] = Nothing
    isKnownEntry name (x : rest) = case x of
        File f | f.name == name -> Just x
        Directory d | baseName d.path == name -> Just x
        _ -> isKnownEntry name rest

readDirectoryEntries :: MonadUnliftIO m => Directory -> m [Entry]
readDirectoryEntries dir = withMVar dir.lock \() -> do
    readTVarIO dir.childs >>= \case
        [] -> readDirectory dir
        xs -> pure xs

readEntry :: MonadIO m => Directory -> RawFilePath -> m (Maybe Entry)
readEntry parent epath = do
    liftIO (try $ getFileStatus epath) >>= \case
        Left (_ :: SomeException) -> pure Nothing
        Right stat
            | isRegularFile stat -> Just . File . MkFile (baseName epath) <$> newTVarIO (fileSize stat)
            | isDirectory stat -> do
                let dpath = removeTrailingSlash epath
                dir <- MkDirectory dpath (Just parent) <$> newTVarIO [] <*> newMVar ()
                pure $ Just $ Directory dir
            | otherwise -> pure Nothing

removeTrailingSlash :: RawFilePath -> RawFilePath
removeTrailingSlash fp
    | "/" `BS.isSuffixOf` fp = removeTrailingSlash (BS.dropEnd 1 fp)
    | otherwise = fp

-- | The absolute location of a directory, for remote user.
newtype FileLoc = FileLoc Text
    deriving newtype (Show, ToJSON, FromJSON, Serialise, Semigroup, Monoid)

instance From FileLoc Text where
    from = coerce

-- | Get the absolute location of a directory.
getFileLoc :: Directory -> Maybe File -> FileLoc
getFileLoc baseDir mFile = mkFileLoc (fileComponent $ dnComponents baseDir)
  where
    fileComponent = case mFile of
        Nothing -> id
        Just file -> (file.name :)
    mkFileLoc = FileLoc . decodeUtf8 . BS.intercalate "/" . reverse
    dnComponents dir = case dir.parent of
        Nothing -> []
        Just parent -> baseName dir.path : dnComponents parent

resolveFileLoc :: MonadUnliftIO m => Directory -> FileLoc -> m (Maybe (Directory, Maybe File))
resolveFileLoc baseDir (FileLoc dn) = findEntry baseDir dnComponents
  where
    dnComponents
        | dn == "" = []
        | otherwise = encodeUtf8 <$> Text.split (== '/') dn
    findEntry dir [] = pure (Just (dir, Nothing))
    findEntry dir (x : xs) = do
        childs <- readDirectoryEntries dir
        case filter (isEntry x dir) childs of
            (Directory d : _) -> findEntry d xs
            (File f : _) -> case xs of
                [] -> pure (Just (dir, Just f))
                _ -> pure Nothing
            _ -> pure Nothing
    isEntry name parent = \case
        Directory dir -> dir.path == mconcat [parent.path, "/", name]
        File file -> file.name == name

lookupChild :: MonadUnliftIO m => Directory -> FileName -> m (Maybe Entry)
lookupChild baseDir (FileName (encodeUtf8 -> name)) = findChild <$> readDirectoryEntries baseDir
  where
    findChild [] = Nothing
    findChild (x : rest) = case x of
        File file | file.name == name -> pure x
        Directory dir | baseName dir.path == name -> pure x
        _ -> findChild rest

renameEntry :: MonadIO m => Directory -> Entry -> FileName -> m ()
renameEntry baseDir entry (FileName (encodeUtf8 -> newName)) = do
    ensureDirectory baseDir
    case entry of
        Directory dir -> do
            let newPath = dirName dir.path <> newName
            -- TODO: check if dest exist
            liftIO $ rename dir.path newPath
            atomically (modifyTVar' baseDir.childs (map (renameDir dir.path newPath)))
        File file -> do
            let oldPath = mconcat [baseDir.path, "/", file.name]
                newPath = mconcat [baseDir.path, "/", newName]
            liftIO $ rename oldPath newPath
            atomically (modifyTVar' baseDir.childs (map (renameFile file.name)))
  where
    renameDir oldPath newPath = \case
        Directory (MkDirectory dpath parent childs lock) | dpath == oldPath -> Directory (MkDirectory newPath parent childs lock)
        e -> e
    renameFile oldName = \case
        File (MkFile fname sz) | fname == oldName -> File (MkFile newName sz)
        e -> e

moveEntry :: MonadIO m => Directory -> Directory -> Entry -> m ()
moveEntry src dst entry = do
    ensureDirectory dst
    case entry of
        Directory dir -> do
            let newPath = mconcat [dst.path, "/", baseName dir.path]
            -- TODO: check if dest exist
            liftIO $ rename dir.path newPath
            atomically do
                modifyTVar' src.childs (removeDir dir)
                modifyTVar' dst.childs (Directory (MkDirectory newPath (Just dst) dir.childs dir.lock) :)
        File file -> do
            let oldPath = mconcat [src.path, "/", file.name]
                newPath = mconcat [dst.path, "/", file.name]
            liftIO $ rename oldPath newPath
            atomically do
                modifyTVar' src.childs (removeFile file)
                addFile dst file
  where
    removeDir dir = filter notDir
      where
        notDir = \case
            Directory d -> d.path /= dir.path
            File{} -> True
    removeFile file = filter notFile
      where
        notFile = \case
            File f -> f.name /= file.name
            Directory{} -> True

data PartialFile = PartialFile
    { dir :: Directory
    , fileName :: RawFilePath
    , tempPath :: FilePath
    }

instance ToJSON PartialFile where
    toJSON pf = toJSON pf.tempPath

-- | FileName are provided by the user, they must not begin or end with a '/'.
newtype FileName = FileName Text
    deriving newtype (Show, Eq, Ord, ToJSON, IsString, ToHtml, Semigroup, Monoid)

instance FromJSON FileName where
    parseJSON = withText "FileName" \txt -> do
        if "/" `Text.isPrefixOf` txt || "/" `Text.isSuffixOf` txt
            then fail "file name must not begin or end with a '/'"
            else pure $ FileName txt

instance From FileName RawFilePath where
    from (FileName n) = encodeUtf8 n

instance From RawFilePath FileName where
    from = FileName . decodeUtf8 . baseName

instance From FileName FileLoc where
    from = coerce

instance From FileName Text where
    from = coerce

createPartialFile :: MonadIO m => Directory -> FileName -> m PartialFile
createPartialFile dir (FileName name) = do
    let fp = from $ mconcat [decodeUtf8 dir.path, "/", name, ".part"]
    liftIO do
        ensureDirectory dir
        BS.writeFile fp ""
    pure $ PartialFile dir (encodeUtf8 name) fp

ensureDirectory :: MonadIO m => Directory -> m ()
ensureDirectory dir =
    liftIO $ createDirectoryIfMissing True (from $ decodeUtf8 dir.path)

appendPartialFile :: MonadIO m => PartialFile -> ByteString -> m ()
appendPartialFile pf = liftIO . BS.appendFile pf.tempPath

finalizePartialFile :: MonadIO m => PartialFile -> FileOffset -> m File
finalizePartialFile pf size = do
    let rfp = encodeUtf8 (from pf.tempPath)
    liftIO $ rename rfp (BS.dropEnd 5 rfp)
    file <- MkFile pf.fileName <$> newTVarIO size
    let notPartFile :: Entry -> Bool
        notPartFile = \case
            File f -> f.name /= file.name <> ".part"
            _ -> True
    atomically do
        addFile pf.dir file
        modifyTVar' pf.dir.childs (filter notPartFile)
    pure file

data ContentType
    = BinaryContent
    | ImageContent
    | VideoContent
    | TextContent
    | PdfContent
    | ArchiveContent
    deriving (Enum, Eq, Ord, Show)

allContentTypeExtensions :: [(ContentType, [RawFilePath])]
allContentTypeExtensions =
    [ (ImageContent, ["png", "jpg", "jpeg", "gif"])
    , (VideoContent, ["webm", "mkv", "avi", "mp4"])
    , (TextContent, ["txt", "md", "hs", "js", "py"])
    , (PdfContent, ["pdf"])
    , (ArchiveContent, ["zip", "tar"])
    ]

takeExtension :: RawFilePath -> RawFilePath
takeExtension fp = case BS.takeWhileEnd (/= unsafeFrom (fromEnum '.')) fp of
    "gz" -> takeExtension (BS.dropEnd 3 fp)
    "bz2" -> takeExtension (BS.dropEnd 4 fp)
    ext -> ext

fileContentType :: File -> ContentType
fileContentType file = getFileContent allContentTypeExtensions
  where
    ext = takeExtension file.name
    getFileContent [] = BinaryContent
    getFileContent ((content, exts) : rest)
        | ext `elem` exts = content
        | otherwise = getFileContent rest
