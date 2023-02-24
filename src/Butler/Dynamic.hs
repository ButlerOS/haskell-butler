-- | This module contains a small wrapper around Data.Dynamic.
module Butler.Dynamic (
    Dynamics,
    newDynamics,
    addDynamic,
    delDynamic,
    withDynamic,
    lookupDynamic,
    waitDynamic,
) where

import Butler.Clock
import Butler.Prelude
import Data.Map.Strict qualified as Map

newtype DynamicsAddr = DynamicsAddr ShortByteString
    deriving newtype (IsString, Ord, Eq, Show)

newtype Dynamics = Dynamics (TVar (Map DynamicsAddr Dynamic))

newDynamics :: STM Dynamics
newDynamics = Dynamics <$> newTVar mempty

addDynamic :: Typeable a => Dynamics -> DynamicsAddr -> a -> STM ()
addDynamic (Dynamics tv) addr = modifyTVar' tv . Map.insert addr . toDyn

delDynamic :: Dynamics -> DynamicsAddr -> STM ()
delDynamic (Dynamics tv) addr = modifyTVar' tv (Map.delete addr)

-- | Wrap add/del around the action.
withDynamic :: (MonadUnliftIO m, Typeable a) => Dynamics -> DynamicsAddr -> a -> m b -> m b
withDynamic dyns addr v action = do
    atomically (addDynamic dyns addr v)
    action `finally` atomically (delDynamic dyns addr)

lookupDynamic :: Typeable a => Dynamics -> DynamicsAddr -> STM (Maybe a)
lookupDynamic (Dynamics tv) addr = (fromDynamic <=< Map.lookup addr) <$> readTVar tv

waitDynamic :: (MonadIO m, Typeable a) => Milli -> Dynamics -> DynamicsAddr -> m (WaitResult a)
waitDynamic maxWait dyns addr = atomically =<< waitTransaction maxWait getDynamic
  where
    getDynamic = do
        lookupDynamic dyns addr >>= \case
            Nothing -> retrySTM
            Just v -> pure v
