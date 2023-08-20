-- | This module contains the logic to interface with unix system
module Butler.UnixShell where

import Butler.Prelude

newtype ImageName = ImageName Text
    deriving newtype (IsString)
instance From ImageName Text where from = coerce

data IsolationRuntime = None | Bubblewrap | Podman ImageName

data Isolation = Isolation
    { runtime :: IsolationRuntime
    , toolbox :: Maybe FilePath
    }

getIsolation :: MonadIO m => m Isolation
getIsolation = do
    runtime <-
        liftIO (getEnv "BUTLER_ISOLATION") >>= \case
            Just "none" -> pure None
            _ -> pure Bubblewrap
    toolbox <- fmap (into @String . decodeUtf8) <$> liftIO (getEnv "BUTLER_TOOLS")
    pure $ Isolation runtime toolbox
