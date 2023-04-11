{- | This module contains the logic to define application runtime instance identifier.

A AppID is unique per 'AppSharedContext'.
-}
module Butler.AppID (
    AppID,
    shellAppID,
    decodeAppID,
    decodeAppIDMessage,
    AppIDCounter,
    newAppIDCounter,
    nextAppID,
) where

import Butler.Core.NatMap qualified as NM
import Butler.Display.GUI (decodeNaturalSuffix)
import Butler.Frame (decodeMessage)
import Butler.Prelude
import Data.Map.Strict qualified as Map

shellAppID :: AppID
shellAppID = AppID 0

newtype AppID = AppID Natural
    deriving newtype (Eq, Ord, Show, ToJSON, Serialise, FromJSON)

decodeAppID :: Text -> Maybe (AppID, Text)
decodeAppID txt = first AppID <$> decodeNaturalSuffix txt

decodeAppIDMessage :: LByteString -> Maybe (AppID, LByteString)
decodeAppIDMessage buf = first AppID <$> decodeMessage buf

instance From AppID Natural where
    from (AppID n) = n

-- This will crash for the application number 2e9, which should not happen before reaching
-- a limitation somewhere else in the system. Thus it's ok to use 'unsafeFrom'.
instance From AppID Int where
    from (AppID n) = unsafeFrom n

newtype AppIDCounter = AppIDCounter NM.NatCounter

newAppIDCounter :: STM AppIDCounter
newAppIDCounter = AppIDCounter <$> NM.newNatCounter

nextAppID :: AppIDCounter -> Map AppID a -> STM AppID
nextAppID (AppIDCounter nc) apps = getNextAppID
  where
    getNextAppID = do
        appID <- AppID <$> NM.incr nc
        case Map.lookup appID apps of
            Nothing -> pure appID
            Just{} -> getNextAppID
