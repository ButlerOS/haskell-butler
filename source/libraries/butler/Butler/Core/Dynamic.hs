-- | This module contains a small wrapper around Data.Dynamic.
module Butler.Core.Dynamic (
    Dynamics,
    newDynamics,
    addDynamic,
    delDynamic,
    getSharedDynamic,
    withDynamic,
    lookupDynamic,
    waitDynamic,
) where

import Butler.Core.Clock
import Butler.Prelude
import Data.Map.Strict qualified as Map

newtype DynamicsAddr = DynamicsAddr ShortByteString
    deriving newtype (IsString, Ord, Eq, Show)

data Dynamics = Dynamics
    { tv :: TVar (Map DynamicsAddr Dynamic)
    , lock :: MVar ()
    }

newDynamics :: MonadIO m => m Dynamics
newDynamics = Dynamics <$> newTVarIO mempty <*> newMVar ()

addDynamic :: Typeable a => Dynamics -> DynamicsAddr -> a -> STM ()
addDynamic (Dynamics tv _lock) addr = modifyTVar' tv . Map.insert addr . toDyn

delDynamic :: Dynamics -> DynamicsAddr -> STM ()
delDynamic (Dynamics tv _lock) addr = modifyTVar' tv (Map.delete addr)

-- | Wrap add/del around the action.
withDynamic :: (MonadUnliftIO m, Typeable a) => Dynamics -> DynamicsAddr -> a -> m b -> m b
withDynamic dyns addr v action = do
    atomically (addDynamic dyns addr v)
    action `finally` atomically (delDynamic dyns addr)

-- | Store the value in dynamics and ensure a single instance can only exist.
getSharedDynamic :: (MonadUnliftIO m, Typeable a) => Dynamics -> DynamicsAddr -> m a -> m a
getSharedDynamic dynamics addr mkValue = withMVar dynamics.lock \() -> do
    atomically (Map.lookup addr <$> readTVar dynamics.tv) >>= \case
        Just v -> case fromDynamic v of
            Just value -> pure value
            Nothing -> error "Invalid dynamic"
        Nothing -> do
            value <- mkValue
            atomically (modifyTVar' dynamics.tv $ Map.insert addr (toDyn value))
            pure value

lookupDynamic :: Typeable a => Dynamics -> DynamicsAddr -> STM (Maybe a)
lookupDynamic (Dynamics tv _lock) addr = (fromDynamic <=< Map.lookup addr) <$> readTVar tv

waitDynamic :: (MonadIO m, Typeable a) => Milli -> Dynamics -> DynamicsAddr -> m (WaitResult a)
waitDynamic maxWait dyns addr = atomically =<< waitTransaction maxWait getDynamic
  where
    getDynamic = do
        lookupDynamic dyns addr >>= \case
            Nothing -> retrySTM
            Just v -> pure v
