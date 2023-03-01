module Butler.Auth.Guest (guestAuthApp) where

import Lucid
import Network.HTTP.Types.Status qualified as HTTP
import Network.Wai qualified

import Butler.Display
import Butler.Display.Session
import Butler.Prelude

guestAuthApp :: Sessions -> Html () -> AuthApplication
guestAuthApp sessions html = AuthApplication app getSession
  where
    app req resp =
        resp $ case Network.Wai.rawPathInfo req of
            "/" -> Network.Wai.responseLBS HTTP.status200 [] (renderBS html)
            _ -> Network.Wai.responseLBS HTTP.status404 mempty mempty
    getSession = \case
        Nothing -> createSession sessions "guest" Nothing
        Just sessionID -> atomically $ lookupSession sessions sessionID
