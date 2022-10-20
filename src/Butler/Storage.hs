module Butler.Storage (
    Storage,
    newStorage,
    scopeStorage,
    syncThread,

    -- * Read/Write
    StorageAddress (..),
    writeStorage,
    readStorage,
) where

import Data.ByteString qualified as BS
import Data.Map qualified as Map
import System.Directory (createDirectoryIfMissing)
import System.Posix.ByteString (RawFilePath)
import System.Posix.Files.ByteString (fileExist)
import Prelude hiding (readFile, writeFile)

import Butler.Clock
import Butler.Prelude

newtype StorageAddress = StorageAddress ByteString
  deriving newtype (Eq, Ord, Serialise, IsString, Semigroup)
  deriving (Show)

instance From Text StorageAddress where
  from = StorageAddress . from

data Storage = Storage
    { rootDir :: RawFilePath
    , journal :: TVar (Map RawFilePath LByteString)
    , sync :: TMVar ()
    }

prepareRootDir :: MonadIO m => RawFilePath -> m RawFilePath
prepareRootDir rootDir = liftIO do
    createDirectoryIfMissing True (unsafeFrom rootDir)
    pure dir
  where
    dir
        | "/" `BS.isSuffixOf` rootDir = rootDir
        | otherwise = rootDir <> "/"

newStorage :: MonadIO m => RawFilePath -> m Storage
newStorage rootDir = do
    dir <- prepareRootDir rootDir
    atomically $ newStorageSTM dir

scopeStorage :: MonadIO m => Storage -> StorageAddress -> m Storage
scopeStorage storage (StorageAddress path) = do
  dir <- prepareRootDir (storage.rootDir <> path)
  pure $ storage { rootDir = dir}

newStorageSTM :: RawFilePath -> STM Storage
newStorageSTM rootDir = do
    Storage rootDir <$> newTVar mempty <*> newEmptyTMVar

-- | The syncThread must be started for the storage to persist
syncThread :: MonadIO m => Storage -> (Int -> m ()) -> m Void
syncThread storage fire = forever do
    atomically $ takeTMVar storage.sync
    sleep 5_000
    contents <- atomically do
        -- flush request added in between
        void $ tryTakeTMVar storage.sync
        Map.toList <$> (stateTVar storage.journal $ \journal -> (journal, mempty))
    fire $ length contents
    traverse_ (liftIO . writeJournal) contents
  where
    writeJournal :: (RawFilePath, LByteString) -> IO ()
    writeJournal (path, content) = BS.writeFile (unsafeFrom path) (from content)

getStoragePath :: RawFilePath -> StorageAddress -> RawFilePath
getStoragePath rootDir (StorageAddress n) = rootDir <> n

readStorage :: MonadIO m => Storage -> StorageAddress -> m (Maybe LByteString)
readStorage storage addr = liftIO do
    let fp = getStoragePath storage.rootDir addr
    fileExist fp >>= \case
        False -> pure Nothing
        True -> Just . from <$> BS.readFile (unsafeFrom fp)

writeStorage :: Storage -> StorageAddress -> LByteString -> STM ()
writeStorage storage addr obj = do
    modifyTVar' storage.journal (Map.insert (getStoragePath storage.rootDir addr) obj)
    void $ tryPutTMVar storage.sync ()
