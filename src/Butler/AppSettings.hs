-- | This module contains the logic for application settings.
module Butler.AppSettings where

import Data.Map.Strict qualified as Map

import Butler.Core
import Butler.Core.Clock
import Butler.Core.Dynamic
import Butler.Core.Memory
import Butler.Prelude

newtype SettingValue = SettingValue Text
    deriving newtype (Serialise, Show, IsString, FromJSON, ToJSON, ToHtml, Ord, Eq)
instance From SettingValue Text where from = coerce

newtype SettingKey = SettingKey Text
    deriving newtype (Serialise, Show, IsString, FromJSON, ToJSON, ToJSONKey, ToHtml, Ord, Eq)
instance From SettingKey Text where from = coerce

data SettingSchema
    = SettingChoice [SettingValue]
    | SettingToggle
    | SettingURL
    | SettingToken

data AppSetting = AppSetting
    { name :: SettingKey
    , value :: SettingValue
    , schema :: SettingSchema
    }

-- | Single app setting saved by the user.
newtype Setting = Setting {settingMap :: Map SettingKey SettingValue}
    deriving newtype (Serialise)

-- | All the apps setting saved by the user.
newtype Settings = Settings {settingsMap :: Map ProgramName Setting}
    deriving newtype (Serialise, Semigroup, Monoid)

-- | The shell process must call this function to enable 'getSettings' and 'lookupSetting'.
withSettings :: Dynamics -> ProcessIO a -> ProcessIO a
withSettings dynamics action = do
    settings <- snd <$> newProcessMemory @Settings "settings" (pure mempty)
    withDynamic dynamics "settings" settings action

-- | Get the global 'Settings'.
getSettings :: MonadUnliftIO m => Dynamics -> m (MemoryVar Settings)
getSettings dynamics = do
    waitDynamic 150 dynamics "settings" >>= \case
        WaitCompleted settings -> pure settings
        WaitTimeout -> error "settings are not available"

-- | Retrieve a single application settings.
lookupSetting :: MemoryVar Settings -> ProgramName -> STM (Map SettingKey SettingValue)
lookupSetting mvSettings program = getSetting program <$> readMemoryVar mvSettings

getSetting :: ProgramName -> Settings -> Map SettingKey SettingValue
getSetting program settings = case Map.lookup program settings.settingsMap of
    Nothing -> mempty
    Just m -> m.settingMap

putSetting :: ProgramName -> SettingKey -> Maybe SettingValue -> MemoryVar Settings -> STM ()
putSetting program setting mValue mv = modifyMemoryVar mv \settings ->
    let newSetting = case Map.lookup program settings.settingsMap of
            Nothing -> case mValue of
                Nothing -> mempty
                Just value -> Map.fromList [(setting, value)]
            Just prev -> case mValue of
                Nothing -> Map.delete setting prev.settingMap
                Just value -> Map.insert setting value prev.settingMap
     in Settings $ Map.insert program (Setting newSetting) settings.settingsMap
