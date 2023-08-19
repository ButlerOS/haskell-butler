module Butler.Display.WebSocket (
    WebSocketAPI,
    OnConnect,
    websocketServer,
    ChannelName (..),
    Workspace (..),
    workspaceUrl,
) where

import Lucid
import Network.HTTP.Types.Status qualified as HTTP
import Network.Socket
import Network.Wai qualified as Wai
import Network.Wai.Handler.WebSockets qualified as WaiWS
import Network.WebSockets qualified as WS

import Servant
import Servant.Auth qualified as SA
import Servant.Auth.Server qualified as SAS

import Butler.Core
import Butler.Core.Storage
import Butler.Prelude

newtype Workspace = Workspace Text
    deriving newtype (Serialise, Eq, Ord, Show, IsString, FromHttpApiData)
    deriving (FromJSON, ToJSON, ToHtml) via Text

instance From Workspace Text where
    from (Workspace n) = n

instance From Workspace StorageAddress where
    from (Workspace n) = StorageAddress (encodeUtf8 n)

{- | Return the absolute workspace url

>>> workspaceUrl Nothing
"/"
>>> workspaceUrl (Just "tasty")
"/tasty/"
-}
workspaceUrl :: Maybe Workspace -> Text
workspaceUrl = \case
    Nothing -> "/"
    Just ws -> "/" <> into ws <> "/"

newtype ChannelName = ChannelName Text
    deriving newtype (Eq, Show, Ord, FromHttpApiData, ToJSON, IsString)

type WebSocketAPI auth = RemoteHost :> SA.Auth '[SA.JWT, SA.Cookie] auth :> Raw

type OnConnect session = SockAddr -> Workspace -> ChannelName -> session -> WS.Connection -> ProcessIO ()

type GetSession auth session = Maybe auth -> ProcessIO (Maybe session)

-- | Look for the "?reconnect=true" argument in the query string
getReconnectArg :: Wai.Request -> Bool
getReconnectArg req = case lookup "reconnect" (Wai.queryString req) of
    Just (Just "true") -> True
    _ -> False

websocketServer :: ToJSON auth => ProcessEnv -> GetSession auth session -> OnConnect session -> Server (WebSocketAPI auth)
websocketServer env getSession onConnect clientAddr auth = Tagged baseApp
  where
    baseApp :: Wai.Application
    baseApp req resp = case Wai.pathInfo req of
        ["ws", ChannelName -> chan] -> handleWS (Workspace "") chan
        [Workspace -> ws, "ws", ChannelName -> chan] -> handleWS ws chan
        _ -> resp404
      where
        resp404 = resp $ Wai.responseLBS HTTP.status404 [] mempty
        handleWS workspace chan = maybe resp404 resp (WaiWS.websocketsApp WS.defaultConnectionOptions wsApp req)
          where
            wsApp :: WS.PendingConnection -> IO ()
            wsApp pendingConnection = do
                conn <- WS.acceptRequest pendingConnection
                runProcessIO env.os env.process (doHandleWS (getReconnectArg req) workspace chan conn)

    doHandleWS :: Bool -> Workspace -> ChannelName -> WS.Connection -> ProcessIO ()
    doHandleWS reconnect workspace chan conn
        | reconnect = doReload
        | otherwise = do
            case auth of
                SAS.Authenticated sessionID -> do
                    mSession <- getSession (Just sessionID)
                    case mSession of
                        Just session -> onConnect clientAddr workspace chan session conn
                        Nothing -> do
                            logError "Unknown websocket session" ["session" .= sessionID]
                            doReload
                _ -> do
                    logError "Unauthenticated websocket connection" []
                    doReload
      where
        doReload = liftIO $ WS.sendTextData conn $ renderText do
            with span_ [id_ "display-ws"] do
                "<reconnecting...>"
                script_ "window.location.reload()"
