module Butler.WebSocket (
    WebSocketAPI,
    OnWSConnect,
    websocketServer,
    splashHtml,
    ChannelName (..),
    Workspace (..),
) where

import Lucid
import Network.Socket
import Network.WebSockets qualified as WS
import Servant
import Servant.API.WebSocket

import Butler.GUI
import Butler.Prelude
import Butler.Session

newtype Workspace = Workspace Text
    deriving newtype (Serialise, Eq, Ord, Show, IsString, FromHttpApiData)
    deriving (FromJSON, ToJSON, ToHtml) via Text

instance From Workspace Text where
    from (Workspace n) = n

type ClientWSAPI = "ws" :> Capture "channel" ChannelName :> QueryParam "reconnect" Bool :> QueryParam "session" SessionID :> WebSocket

type ClientAPI = ClientWSAPI :<|> (Capture "workspace" Workspace :> ClientWSAPI)

clientServer :: GetSession session -> (Workspace -> ChannelName -> session -> WS.Connection -> ProcessIO ()) -> ServerT ClientAPI ProcessIO
clientServer getSession onConnect = connectRoute Nothing :<|> connectRoute . Just
  where
    connectRoute :: Maybe Workspace -> ChannelName -> Maybe Bool -> Maybe SessionID -> WS.Connection -> ProcessIO ()
    connectRoute workspaceM name (fromMaybe False -> reconnect) sessionIDM connection
        | reconnect = doReload
        | otherwise = do
            isSessionValid <- getSession sessionIDM
            case isSessionValid of
                Nothing -> doReload
                Just session -> onConnect workspace name session connection
      where
        workspace = case workspaceM of
            Just ws -> ws
            Nothing -> Workspace ""
        doReload = liftIO $ WS.sendTextData connection $ renderText do
            with span_ [id_ "display-ws"] do
                "<reconnecting...>"
                script_ "window.location.reload()"

newtype ChannelName = ChannelName Text
    deriving newtype (Eq, Show, Ord, FromHttpApiData, ToJSON, IsString)

type WebSocketAPI = RemoteHost :> ClientAPI

type OnWSConnect session = SockAddr -> Workspace -> ChannelName -> session -> WS.Connection -> ProcessIO ()

type GetSession session = Maybe SessionID -> ProcessIO (Maybe session)

websocketServer :: GetSession session -> OnWSConnect session -> ServerT WebSocketAPI ProcessIO
websocketServer getSession onWSConnect clientAddr = clientServer getSession (onWSConnect clientAddr)
