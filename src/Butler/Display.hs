module Butler.Display (
    Display (..),
    AuthApplication (..),
    OnClient,
    startDisplay,
    getClient,
    DisplayEvent (..),
    JwkStorage (..),
    module Butler.DisplayClient,
) where

import Codec.Serialise.Decoding (decodeBytes)
import Codec.Serialise.Encoding (encodeBytes)
import Crypto.JOSE.JWK qualified as JOSE
import Data.Aeson (decodeStrict')
import Data.List (find)
import Data.Map.Strict qualified as Map
import Lucid
import Lucid.Htmx
import Network.HTTP.Types.Status qualified as HTTP
import Network.Socket
import Network.Wai qualified
import Network.WebSockets qualified as WS
import Servant

import Butler.DisplayClient
import Butler.Network
import Butler.OS
import Butler.Pipe (BroadcastChan (..), broadcast, newBroadcastChan)
import Butler.Prelude
import Butler.Process
import Butler.Session
import Butler.WebSocket

data Display = Display
    { sessions :: Sessions
    , clients :: TVar (Map SessionID [DisplayClient])
    , events :: BroadcastChan DisplayEvent
    }

type OnClient = (Workspace -> ProcessIO (ProcessEnv, DisplayEvent -> ProcessIO ()))

newDisplay :: Sessions -> STM Display
newDisplay sessions = Display sessions <$> newTVar mempty <*> newBroadcastChan

addDisplayClient :: Display -> DisplayClient -> STM ()
addDisplayClient display client = do
    let alter = \case
            Just clients -> Just (client : clients)
            Nothing -> Just [client]
    modifyTVar' display.clients (Map.alter alter client.session.sessionID)

getClient :: Display -> SessionID -> Endpoint -> STM (Maybe DisplayClient)
getClient display session endpoint = do
    sessions <- readTVar display.clients
    pure $ case Map.lookup session sessions of
        Just clients -> Data.List.find (\c -> c.endpoint == endpoint) clients
        Nothing -> Nothing

removeClient :: Display -> SessionID -> Endpoint -> STM ()
removeClient display session endpoint = do
    let alter = \case
            Just clients ->
                let newClients = filter (\c -> c.endpoint /= endpoint) clients
                 in case newClients of
                        [] -> Nothing
                        xs -> Just xs
            Nothing -> Nothing
    modifyTVar' display.clients (Map.alter alter session)

data DisplayEvent
    = UserConnected ChannelName DisplayClient
    | UserDisconnected ChannelName DisplayClient
    deriving (Generic, ToJSON)

instance Show DisplayEvent where
    show = \case
        UserConnected{} -> "UserConnected"
        UserDisconnected{} -> "UserDisconnected"

dcSplash :: Html ()
dcSplash = do
    with div_ [class_ "absolute top-0 left-0 z-50 bg-slate-200/80 flex h-screen w-screen"] do
        with div_ [class_ "m-auto text-xl"] "Thanks for your time, see you next time!"
        script_ "htmx.addClass(htmx.find('#display-pulse'), 'bg-red-500')"

newtype JwkStorage = JwkStorage JOSE.JWK

instance Serialise JwkStorage where
    encode (JwkStorage jwk) = encodeBytes (from $ encodeJSON jwk)
    decode = fmap decodeJWK decodeBytes
      where
        decodeJWK :: ByteString -> JwkStorage
        decodeJWK bs = JwkStorage (fromMaybe (error "bad encoding?!") $ decodeStrict' bs)

connectRoute :: Display -> OnClient -> SockAddr -> Workspace -> ChannelName -> Session -> TabID -> WS.Connection -> ProcessIO ()
connectRoute display onClient sockAddr workspaceM channel session tabID connection = do
    -- TODO: check if the ping thread is necessary
    -- WS.withPingThread connection 30 (pure ()) do

    let clientAddr = from $ show sockAddr
        ChannelName cn = channel
        progName = "client-" <> cn
        name = ProgramName $ progName <> "-" <> clientAddr
        endpoint = Endpoint clientAddr
    (processEnv, handler) <- onClient workspaceM
    clientM <- newEmptyMVar
    clientProcess <- asProcess processEnv $ spawnProcess name do
        clientProcess <- getSelfProcess
        client <- newClient connection endpoint clientProcess session tabID
        putMVar clientM client
        -- Add the client to server state
        let ev = UserConnected channel client
        atomically do
            addDisplayClient display client
            broadcast display.events ev
        handler ev

    client <- takeMVar clientM
    -- Wait for client completion
    res <- atomically $ await clientProcess.thread

    -- Remove the client from the server state
    let ev = UserDisconnected channel client
    atomically do
        removeClient display session.sessionID endpoint
        broadcast display.events ev
    asProcess processEnv $ handler ev
    logInfo "Client quit" ["endpoint" .= endpoint, "reason" .= into @Text res]

    -- Say goodbye
    case res of
        Killed -> liftIO do
            -- TODO: check for SocketClosed exception here
            WS.sendTextData connection $ renderText do
                with div_ [id_ "display-root", class_ "h-full w-full", hxSwapOob_ "afterbegin"] dcSplash
            WS.sendClose connection ("see you next time!" :: ByteString)
        _ -> pure ()

data AuthApplication = AuthApplication
    { app :: WaiApplication
    , getSession :: Maybe SessionID -> ProcessIO (Maybe Session)
    }

startDisplay :: Port -> [XStaticFile] -> (Sessions -> ProcessIO AuthApplication) -> (Display -> ProcessIO OnClient) -> ProcessIO Void
startDisplay port xfiles mkAuthApp withDisplay = do
    sessions <- loadSessions
    display <- atomically (newDisplay sessions)
    authApp <- mkAuthApp sessions
    onClient <- withDisplay display
    env <- ask
    let wsSrv :: ServerT WebSocketAPI ProcessIO
        wsSrv = websocketServer authApp.getSession (connectRoute display onClient)
        wsApp :: WaiApplication
        wsApp = Servant.serveWithContextT (Proxy @WebSocketAPI) EmptyContext (liftIO . runProcessIO env.os env.process) wsSrv

        glApp req resp =
            let wsRespHandler wsResp = case HTTP.statusCode (Network.Wai.responseStatus wsResp) of
                    404 -> authApp.app req resp
                    _ -> resp wsResp
             in wsApp req wsRespHandler

    webService xfiles glApp port (Https Nothing)
