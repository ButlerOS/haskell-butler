-- | This module contains a general purpose data structure.
module Butler.History (
    History,
    newHistory,
    addHistory,
    recentHistory,
    oldestHistory,
) where

import Data.Sequence ((|>))
import Data.Sequence qualified as Seq

import Butler.Prelude

data History a = History
    { maxSize :: Natural
    , history :: TVar (Seq a)
    }

newHistory :: Natural -> STM (History a)
newHistory size = History size <$> newTVar mempty

addHistory :: History a -> a -> STM ()
addHistory h a =
    modifyTVar' h.history $ \buffer ->
        let cleanBuffer
                | seqLength buffer > h.maxSize = Seq.drop 1 buffer
                | otherwise = buffer
         in (cleanBuffer |> a)

recentHistory :: History a -> STM [a]
recentHistory h = toList . Seq.reverse <$> readHistory h

oldestHistory :: History a -> STM [a]
oldestHistory h = toList <$> readHistory h

readHistory :: History a -> STM (Seq a)
readHistory h = readTVar h.history

seqLength :: Seq a -> Natural
seqLength = unsafeFrom . Seq.length
