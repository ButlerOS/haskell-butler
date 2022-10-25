module Butler.Auth.Guest (guestAuthApp) where

import Data.UUID.V4 qualified as UUID
import Lucid
import Network.HTTP.Types.Status qualified as HTTP
import Network.Wai qualified

import Butler.Display
import Butler.Prelude
import Butler.Session

guestAuthApp :: Html () -> AuthApplication
guestAuthApp html = AuthApplication app getSession
  where
    app req resp =
        resp $ case Network.Wai.rawPathInfo req of
            "/" -> Network.Wai.responseLBS HTTP.status200 [] (renderBS html)
            _ -> Network.Wai.responseLBS HTTP.status404 mempty mempty
    getSession = \case
        Nothing -> do
            sessionID <- SessionID <$> liftIO UUID.nextRandom
            pure $ Just (Session sessionID "guest")
        Just sessionID -> pure $ Just (Session sessionID "guest")
