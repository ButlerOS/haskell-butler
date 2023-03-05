-- | This module provides a persistant TVar in Bulter storage.
module Butler.Core.Memory (
    MemoryVar,
    newMemoryVar,
    readMemoryVar,
    modifyMemoryVar,
    stateMemoryVar,
) where

import Butler.Core.Storage
import Butler.Prelude

-- | The 'MemoryVar' that wraps the 'TVar'. The TVar content must be an instance of 'Serialise'.
data MemoryVar a = MemoryVar
    { save :: STM ()
    -- ^ A 'STM' action that serialize the 'TVar' to the Butler storage
    , var :: TVar a
    -- ^ The wrapped 'TVar'
    }

-- | Instanciate a new 'MemoryVar' by providing a 'Storage' and a 'StorageAddress'
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

-- | Read the 'MemoryVar'. Use it as a replacement to readTVar.
readMemoryVar :: MemoryVar a -> STM a
readMemoryVar mv = readTVar mv.var

{- | Write into the 'MemoryVar'. The inner 'TVar' is persisted to the Butler storage.
 Use it as a replacement to 'modifyTVar''.
-}
modifyMemoryVar :: MemoryVar a -> (a -> a) -> STM ()
modifyMemoryVar mv f = do
    modifyTVar' mv.var f
    mv.save

{- | Similar to 'modifyMemoryVar' but returning an extra value
 Use it as a replacement to 'stateTVar'
-}
stateMemoryVar :: MemoryVar a -> (a -> (b, a)) -> STM b
stateMemoryVar mv f = do
    res <- stateTVar mv.var f
    mv.save
    pure res
