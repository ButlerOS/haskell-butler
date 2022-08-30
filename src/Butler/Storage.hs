module Butler.Storage (
    Storage,
    newStorage,
    mkDir,
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

newtype StorageAddress = StorageAddress ByteString deriving newtype (Eq, Ord, Serialise, IsString, Semigroup)

data Storage = Storage
    { rootDir :: RawFilePath
    , journal :: TVar (Map StorageAddress LByteString)
    , sync :: TMVar ()
    }

newStorage :: RawFilePath -> IO Storage
newStorage rootDir = do
    createDirectoryIfMissing True (unsafeFrom rootDir)
    atomically $ newStorageSTM dir
  where
    dir
        | "/" `BS.isSuffixOf` rootDir = rootDir
        | otherwise = rootDir <> "/"

mkDir :: MonadIO m => Storage -> StorageAddress -> m StorageAddress
mkDir storage (StorageAddress path) = liftIO do
    createDirectoryIfMissing True (unsafeFrom $ storage.rootDir <> path)
    pure (StorageAddress $ path <> "/")

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
    writeJournal :: (StorageAddress, LByteString) -> IO ()
    writeJournal (addr, content) = do
        BS.writeFile (unsafeFrom $ getStoragePath storage.rootDir addr) (from content)

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
    modifyTVar' storage.journal (Map.insert addr obj)
    void $ tryPutTMVar storage.sync ()
