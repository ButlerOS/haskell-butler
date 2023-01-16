module Butler.DisplayClient where

import Data.ByteString qualified as BS
import Lucid
import Network.WebSockets qualified as WS

import Butler.Clock
import Butler.Frame
import Butler.NatMap qualified as NM
import Butler.OS
import Butler.Prelude
import Butler.Session

data DisplayClient = DisplayClient
    { conn :: WS.Connection
    , endpoint :: Endpoint
    , process :: Process
    , session :: Session
    , recv :: TVar Word64
    , send :: TVar Word64
    }

newClient :: MonadIO m => WS.Connection -> Endpoint -> Process -> Session -> m DisplayClient
newClient c endpoint p s = DisplayClient c endpoint p s <$> newTVarIO 0 <*> newTVarIO 0

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

safeSend :: DisplayClients -> (DisplayClient -> a -> ProcessIO ()) -> DisplayClient -> a -> ProcessIO ()
safeSend clients action client body = do
    res <- try (action client body)
    case res of
        Right () -> pure ()
        Left (err :: WS.ConnectionException) -> do
            atomically $ delClient clients client
            logError "send error" ["client" .= client, "err" .= showT err]

clientsBroadcastMessage :: DisplayClients -> ByteString -> ProcessIO ()
clientsBroadcastMessage clients buf = do
    xs <- atomically (getClients clients)
    forM_ xs $ \client -> safeSend clients sendMessage client buf

clientsBroadcast :: DisplayClients -> HtmlT STM () -> ProcessIO ()
clientsBroadcast clients message = do
    body <- from <$> atomically (renderBST message)
    xs <- atomically (getClients clients)
    forM_ xs $ \client -> safeSend clients sendTextMessage client body

newtype Endpoint = Endpoint Text
    deriving newtype (Eq, Ord, Show, IsString)
    deriving (FromJSON, ToJSON, ToHtml) via Text

instance ToJSON DisplayClient where
    toJSON dc = object ["endpoint" .= dc.endpoint, "session" .= dc.session]

newtype DisplayClients = DisplayClients (NM.NatMap DisplayClient)

newDisplayClients :: STM DisplayClients
newDisplayClients = DisplayClients <$> NM.newNatMap

newDisplayClientsFromClient :: DisplayClient -> STM DisplayClients
newDisplayClientsFromClient client = do
    clients <- newDisplayClients
    addClient clients client
    pure clients

getClients :: DisplayClients -> STM [DisplayClient]
getClients (DisplayClients x) = NM.elems x

addClient :: DisplayClients -> DisplayClient -> STM ()
addClient (DisplayClients x) = void . NM.add x

delClient :: DisplayClients -> DisplayClient -> STM ()
delClient (DisplayClients x) client = NM.nmDelete x (\o -> o.endpoint == client.endpoint)

clientsDraw :: DisplayClients -> (DisplayClient -> ProcessIO (HtmlT STM ())) -> ProcessIO ()
clientsDraw clients draw = do
    xs <- atomically (getClients clients)
    forM_ xs $ \client -> do
        htmlT <- draw client
        body <- from <$> atomically (renderBST htmlT)
        safeSend clients sendTextMessage client body

type HandlerCallback = ByteString -> ChannelID -> DisplayClient -> ByteString -> ProcessIO ()

type Handlers = NM.NatMap HandlerCallback

newHandler :: Handlers -> HandlerCallback -> STM ChannelID
newHandler handlers handl = do
    newChannel <$> NM.add handlers handl
