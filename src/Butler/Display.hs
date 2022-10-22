module Butler.Display (
    Display (..),
    OnClient,
    startDisplay,
    getClient,
    DisplayEvent (..),
    DisplayClient (..),
    Endpoint (..),
    spawnPingThread,
    recvMessage,
    sendMessage,
    sendTextMessage,
    sendHtml,
    safeSend,
    DisplayClients,
    newDisplayClients,
    getClients,
    addClient,
    delClient,
    clientsBroadcast,
    clientsDraw,
) where

import Codec.Serialise.Decoding (decodeBytes)
import Codec.Serialise.Encoding (encodeBytes)
import Crypto.JOSE.JWK qualified as JOSE
import Data.Aeson (decodeStrict')
import Data.ByteString qualified as BS
import Data.List (find)
import Data.Map.Strict qualified as Map
import Data.Time (UTCTime (..), fromGregorian)
import Lucid
import Lucid.Htmx
import Network.Socket
import Network.WebSockets qualified as WS
import Servant (Context (EmptyContext, (:.)), serveWithContextT)
import Servant.Auth.Server (CookieSettings (..), JWTSettings, SameSite (SameSiteStrict), defaultCookieSettings, defaultJWTSettings, generateKey)

import Lucid.XStatic
import XStatic.Butler as XStatic
import XStatic.Htmx qualified as XStatic
import XStatic.Hyperscript qualified as XStatic
import XStatic.NoVNC qualified as XStatic
import XStatic.Remixicon qualified as XStatic
import XStatic.SweetAlert2 qualified as XStatic
import XStatic.Tailwind qualified as XStatic
import XStatic.Winbox qualified as XStatic
import XStatic.Xterm qualified as XStatic

import Butler.Clock
import Butler.NatMap qualified as NM
import Butler.Network
import Butler.OS
import Butler.Pipe (BroadcastChan (..), broadcast, newBroadcastChan)
import Butler.Prelude
import Butler.Process
import Butler.Session
import Butler.WebSocket

data Display = Display
    { sessions :: Sessions
    , jwtSettings :: JWTSettings
    , clients :: TVar (Map SessionID [DisplayClient])
    , events :: BroadcastChan DisplayEvent
    }

type OnClient = (Workspace -> ProcessIO (ProcessEnv, DisplayEvent -> ProcessIO ()))

loadDisplay :: ProcessIO Display
loadDisplay = do
    JwkStorage myKey <- fst <$> newProcessMemory "display-key.jwk" (JwkStorage <$> generateKey)
    let jwtSettings = defaultJWTSettings myKey
    sessions <- loadSessions
    liftIO $ atomically do
        Display sessions jwtSettings <$> newTVar mempty <*> newBroadcastChan

newtype Endpoint = Endpoint Text
    deriving newtype (Eq, Ord, Show, IsString)
    deriving (FromJSON, ToJSON, ToHtml) via Text

data DisplayClient = DisplayClient
    { conn :: WS.Connection
    , channel :: ChannelName
    , endpoint :: Endpoint
    , process :: Process
    , session :: Session
    , recv :: TVar Word64
    , send :: TVar Word64
    }

instance ToJSON DisplayClient where
    toJSON dc = object ["endpoint" .= dc.endpoint, "session" .= dc.session]

newClient :: MonadIO m => WS.Connection -> ChannelName -> Endpoint -> Process -> Session -> m DisplayClient
newClient c mc endpoint p s = DisplayClient c mc endpoint p s <$> newTVarIO 0 <*> newTVarIO 0

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

spawnPingThread :: DisplayClient -> ProcessIO ()
spawnPingThread client = spawnThread_ $ forever do
    sleep 30_000
    liftIO (WS.sendPing client.conn ("ping" :: ByteString))

recvMessage :: MonadIO m => DisplayClient -> m ByteString
recvMessage client = do
    buf <- liftIO $ WS.receiveData client.conn
    atomically $ modifyTVar' client.recv (+ unsafeFrom (BS.length buf))
    pure buf

sendMessage :: MonadIO m => DisplayClient -> ByteString -> m ()
sendMessage client t = liftIO do
    atomically $ modifyTVar' client.send (+ unsafeFrom (BS.length t))
    WS.sendBinaryData client.conn t

sendTextMessage :: MonadIO m => DisplayClient -> ByteString -> m ()
sendTextMessage client t = liftIO do
    atomically $ modifyTVar' client.send (+ unsafeFrom (BS.length t))
    WS.sendTextData client.conn t

sendHtml :: MonadIO m => DisplayClient -> HtmlT STM () -> m ()
sendHtml client msg = liftIO do
    body <- atomically $ renderBST msg
    sendTextMessage client (from body)

newtype DisplayClients = DisplayClients (NM.NatMap DisplayClient)

newDisplayClients :: STM DisplayClients
newDisplayClients = DisplayClients <$> NM.newNatMap

getClients :: DisplayClients -> STM [DisplayClient]
getClients (DisplayClients x) = NM.elems x

addClient :: DisplayClients -> DisplayClient -> STM ()
addClient (DisplayClients x) = void . NM.add x

delClient :: DisplayClients -> DisplayClient -> STM ()
delClient (DisplayClients x) client = NM.nmDelete x (\o -> o.endpoint == client.endpoint)

safeSend :: DisplayClients -> (DisplayClient -> a -> ProcessIO ()) -> DisplayClient -> a -> ProcessIO ()
safeSend clients action client body = do
    res <- try (action client body)
    case res of
        Right () -> pure ()
        Left (err :: WS.ConnectionException) -> do
            atomically $ delClient clients client
            logError "send error" ["client" .= client, "err" .= showT err]

clientsBroadcast :: DisplayClients -> HtmlT STM () -> ProcessIO ()
clientsBroadcast clients message = do
    body <- from <$> atomically (renderBST message)
    xs <- atomically (getClients clients)
    forM_ xs $ \client -> safeSend clients sendTextMessage client body

clientsDraw :: DisplayClients -> (DisplayClient -> ProcessIO (HtmlT STM ())) -> ProcessIO ()
clientsDraw clients draw = do
    xs <- atomically (getClients clients)
    forM_ xs $ \client -> do
        htmlT <- draw client
        body <- from <$> atomically (renderBST htmlT)
        safeSend clients sendTextMessage client body

data DisplayEvent
    = UserConnected ChannelName DisplayClient
    | UserDisconnected ChannelName DisplayClient
    deriving (Generic, ToJSON)

instance Show DisplayEvent where
    show = \case
        UserConnected{} -> "UserConnected"
        UserDisconnected{} -> "UserDisconnected"

indexHtml :: Html () -> Html ()
indexHtml body = do
    doctypehtml_ do
        head_ do
            title_ "Butler"
            meta_ [charset_ "utf-8"]
            meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
            xstaticScripts xfiles
            link_ [rel_ "icon", href_ "/xstatic/favicon.ico"]
            -- fix the reconnect delay to 1sec
            script_ "htmx.config.wsReconnectDelay = (retryCount) => 1000;"

        with body_ [class_ "font-mono cursor-default bg-stone-100 h-screen"] do
            body
            -- Update the status when the connection is dropped
            script_ displayPulseError

displayPulseError :: Text
displayPulseError =
    [raw|
document.body.addEventListener('htmx:wsError', function(evt) {
  try {
    htmx.addClass(htmx.find("#display-pulse"), "bg-red-500");
  } catch (e) {}
});
|]

dcSplash :: Html ()
dcSplash = do
    with div_ [class_ "absolute top-0 left-0 z-50 bg-slate-200/80 flex h-screen w-screen"] do
        with div_ [class_ "m-auto text-xl"] "Thanks for your time, see you next time!"
        script_ "htmx.addClass(htmx.find('#sm-list'), 'hidden')"
        script_ "htmx.addClass(htmx.find('#display-pulse'), 'bg-red-500')"

cookieSettings :: CookieSettings
cookieSettings =
    defaultCookieSettings
        { -- cookieIsSecure = NotSecure
          cookieSameSite = SameSiteStrict
        , cookieXsrfSetting = Nothing
        , cookieExpires = Just (UTCTime (fromGregorian 2030 1 1) 0)
        }

newtype JwkStorage = JwkStorage JOSE.JWK

instance Serialise JwkStorage where
    encode (JwkStorage jwk) = encodeBytes (from $ encodeJSON jwk)
    decode = fmap decodeJWK decodeBytes
      where
        decodeJWK :: ByteString -> JwkStorage
        decodeJWK bs = JwkStorage (fromMaybe (error "bad encoding?!") $ decodeStrict' bs)

connectRoute :: Display -> OnClient -> SockAddr -> Workspace -> ChannelName -> Session -> WS.Connection -> ProcessIO ()
connectRoute display onClient sockAddr workspaceM channel session connection = do
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
        client <- newClient connection channel endpoint clientProcess session
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

startDisplay :: Port -> (Display -> ProcessIO OnClient) -> ProcessIO Void
startDisplay port withDisplay = do
    display <- loadDisplay
    onClient <- withDisplay display
    let cfg = cookieSettings :. display.jwtSettings :. EmptyContext
    let wss =
            WebSocketServer
                { mkIndexHtml = indexHtml
                , jwtSettings = display.jwtSettings
                , onConnect = connectRoute display onClient
                }
        srv = websocketServer display.sessions wss
    env <- ask
    webService
        (XStatic.noVNC <> XStatic.winbox <> xfiles)
        (Servant.serveWithContextT (Proxy @WebSocketAPI) cfg (liftIO . runProcessIO env.os env.process) srv)
        port

xfiles :: [XStaticFile]
xfiles =
    [ XStatic.sweetAlert2
    , XStatic.hyperscript
    , XStatic.htmx
    , XStatic.htmxExtWS
    , XStatic.tailwind
    , XStatic.remixiconCss
    , XStatic.remixiconWoff2
    , XStatic.logo
    , XStatic.xtermFitAddonJs
    , XStatic.xtermFitAddonJsMap
    , XStatic.winboxCss
    ]
        <> XStatic.xterm
