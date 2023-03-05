module Butler.Core.Memory (
    MemoryVar,
    newMemoryVar,
    readMemoryVar,
    modifyMemoryVar,
    stateMemoryVar,
) where

import Butler.Core.Storage
import Butler.Prelude

data MemoryVar a = MemoryVar
    { save :: STM ()
    , var :: TVar a
    }

newMemoryVar :: Serialise a => MonadIO m => Storage -> StorageAddress -> m a -> m (a, MemoryVar a)
newMemoryVar storage addr initialise = do
    bufM <- readStorage storage addr
    value <- case bufM of
        Just buf -> pure $ deserialise (from buf)
        Nothing -> do
            v <- initialise
            atomically $ doSave v
            pure v

    var <- newTVarIO value
    let save = do
            v <- readTVar var
            writeStorage storage addr (serialise v)
    pure (value, MemoryVar save var)
  where
    doSave v = writeStorage storage addr (serialise v)

readMemoryVar :: MemoryVar a -> STM a
readMemoryVar mv = readTVar mv.var

modifyMemoryVar :: MemoryVar a -> (a -> a) -> STM ()
modifyMemoryVar mv f = do
    modifyTVar' mv.var f
    mv.save

stateMemoryVar :: MemoryVar a -> (a -> (b, a)) -> STM b
stateMemoryVar mv f = do
    res <- stateTVar mv.var f
    mv.save
    pure res
