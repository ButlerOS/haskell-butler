module Butler.NatMap (
    NatCounter,
    newNatCounter,
    incr,
    NatMap,
    newNatMap,
    newKey,
    elems,
    elemsIndex,
    Butler.NatMap.lookup,
    delete,
    nmDelete,
    insert,
    add,
    addWithKey,
) where

import Butler.Prelude
import Data.IntMap.Strict qualified as IM

newtype NatCounter = NatCounter (TVar Natural)

newNatCounter :: STM NatCounter
newNatCounter = NatCounter <$> newTVar 0

incr :: NatCounter -> STM Natural
incr (NatCounter nc) = stateTVar nc \v -> let new = v + 1 in (new, new)

data NatMap a = NatMap
    { counter :: NatCounter
    , values :: TVar (IntMap a)
    }

newNatMap :: STM (NatMap a)
newNatMap = NatMap <$> newNatCounter <*> newTVar mempty

newKey :: NatMap a -> STM Natural
newKey nm = incr nm.counter

elems :: NatMap a -> STM [a]
elems nm = fmap snd . IM.toAscList <$> readTVar nm.values

elemsIndex :: NatMap a -> STM [(Natural, a)]
elemsIndex nm = fmap toNatKey . IM.toAscList <$> readTVar nm.values
  where
    toNatKey (k, v) = (unsafeFrom k, v)

nmDelete :: NatMap a -> (a -> Bool) -> STM ()
nmDelete nm f = modifyTVar' nm.values $ IM.filter (not . f)

lookup :: NatMap a -> Natural -> STM (Maybe a)
lookup nm mapKey = IM.lookup (unsafeFrom mapKey) <$> readTVar nm.values

delete :: NatMap a -> Natural -> STM ()
delete nm mapKey = modifyTVar' nm.values (IM.delete (unsafeFrom mapKey))

add :: NatMap a -> a -> STM Natural
add nm value = do
    k <- newKey nm
    insert nm k value
    pure k

addWithKey :: NatMap a -> (Natural -> STM a) -> STM a
addWithKey nm mkValue = do
    k <- newKey nm
    value <- mkValue k
    insert nm k value
    pure value

-- todo: throw error if elem already exist?
insert :: NatMap a -> Natural -> a -> STM ()
insert nm mapKey value = modifyTVar' nm.values (IM.insert (unsafeFrom mapKey) value)
