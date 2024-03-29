-- | This module contains the 'DisplayClient' logic.
module Butler.Display.Client (
    -- * Display client
    DisplayClient (..),

    -- * Html output
    sendHtml,
    sendsHtml,
    sendsHtmlButSelf,
    clientsDraw,
    clientsDrawT,

    -- * Binary output
    sendBinary,
    sendsBinary,
    sendsBinaryButSelf,

    -- * Inputs
    recvData,
    recvBinary,

    -- * Management thread
    sendThread,

    -- * Collection of clients
    DisplayClients,
    newDisplayClients,
    getClients,
    addClient,
    delClient,

    -- * Clients data
    ClientsData,
    newClientsData,
    addClientsData,
    delClientsData,
    getClientsData,
    lookupClientData,
    withClientsData,

    -- * Internal
    newClient,
    Endpoint (..),
) where

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Lucid
import Network.WebSockets qualified as WS

import Butler.Core
import Butler.Core.Clock
import Butler.Core.NatMap qualified as NM
import Butler.Core.Network
import Butler.Display.Session
import Butler.Prelude

-- | A network client. Note that the 'sendThread' must be running for the @send*@ function to work.
data DisplayClient = DisplayClient
    { conn :: WS.Connection
    , endpoint :: Endpoint
    , server :: ServerName
    , process :: Process
    , session :: Session
    , recv :: TVar Word64
    , send :: TVar Word64
    , sendChannel :: TChan WS.DataMessage
    }

newClient :: WS.Connection -> Endpoint -> ServerName -> Process -> Session -> STM DisplayClient
newClient c endpoint server p s = DisplayClient c endpoint server p s <$> newTVar 0 <*> newTVar 0 <*> newTChan

-- | An action to send data message. Send ping after 30sec of inactivity.
sendThread :: DisplayClient -> ProcessIO Void
sendThread client = forever do
    wDataMessage <- atomically =<< waitTransaction 30_000 (readTChan client.sendChannel)
    case wDataMessage of
        WaitTimeout -> liftIO $ WS.sendPing client.conn ("ping" :: ByteString)
        WaitCompleted dataMessage -> do
            atomically $ modifyTVar' client.send (+ unsafeFrom (LBS.length (from dataMessage)))
            liftIO $ WS.sendDataMessage client.conn dataMessage

-- | Low-level helper to read a 'DataMessage'.
recvData :: MonadIO m => DisplayClient -> m WS.DataMessage
recvData client = do
    dataMessage <- liftIO (WS.receiveDataMessage client.conn)
    atomically $ modifyTVar' client.recv (+ unsafeFrom (LBS.length (from dataMessage)))
    pure dataMessage

-- | Low-level helper to read a binary buffer.
recvBinary :: MonadIO m => DisplayClient -> m ByteString
recvBinary client = do
    buf <- liftIO (WS.receiveData client.conn)
    atomically $ modifyTVar' client.recv (+ unsafeFrom (BS.length buf))
    pure buf

-- | Send Html to a client.
sendHtml :: DisplayClient -> HtmlT STM () -> STM ()
sendHtml client htmlT = do
    !body <- renderBST htmlT
    writeTChan client.sendChannel (WS.Text body Nothing)

-- | Send Html to all clients.
sendsHtml :: DisplayClients -> HtmlT STM () -> ProcessIO ()
sendsHtml clients htmlT = do
    !body <- atomically $ renderBST htmlT
    xs <- atomically $ getClients clients
    forM_ xs \client -> atomically (writeTChan client.sendChannel (WS.Text body Nothing))

-- | Send Html to all clients except the provided one (self).
sendsHtmlButSelf :: DisplayClient -> DisplayClients -> HtmlT STM () -> ProcessIO ()
sendsHtmlButSelf self clients htmlT = do
    !body <- atomically $ renderBST htmlT
    xs <- atomically $ getClients clients
    forM_ xs $ \client ->
        when (client.endpoint /= self.endpoint) do
            atomically $ writeTChan client.sendChannel (WS.Text body Nothing)

-- | Send binary to a client.
sendBinary :: DisplayClient -> LByteString -> STM ()
sendBinary client !buf = writeTChan client.sendChannel (WS.Binary buf)

-- | Send binary to all clients.
sendsBinary :: DisplayClients -> LByteString -> ProcessIO ()
sendsBinary clients !buf = do
    xs <- atomically $ getClients clients
    forM_ xs $ \client -> atomically $ sendBinary client buf

-- | Send binary to all clients except the provided one (self).
sendsBinaryButSelf :: DisplayClient -> DisplayClients -> LByteString -> ProcessIO ()
sendsBinaryButSelf self clients !buf = do
    xs <- atomically $ getClients clients
    forM_ xs $ \client ->
        when (client.endpoint /= self.endpoint) do
            atomically $ writeTChan client.sendChannel (WS.Binary buf)

-- | Render and send html to all clients. Use this helper to adapt the output per client.
clientsDrawT :: DisplayClients -> (DisplayClient -> HtmlT STM ()) -> ProcessIO ()
clientsDrawT clients draw = clientsDraw clients (pure . draw)

clientsDraw :: DisplayClients -> (DisplayClient -> ProcessIO (HtmlT STM ())) -> ProcessIO ()
clientsDraw clients draw = do
    xs <- atomically (getClients clients)
    forM_ xs $ \client -> do
        htmlT <- draw client
        !body <- atomically $ renderBST htmlT
        atomically $ writeTChan client.sendChannel (WS.Text body Nothing)

newtype Endpoint = Endpoint Text
    deriving newtype (Eq, Ord, Show, IsString, ToJSONKey)
    deriving (FromJSON, ToJSON, ToHtml) via Text

instance ToJSON DisplayClient where
    toJSON dc = object ["endpoint" .= dc.endpoint, "session" .= dc.session]

-- | A collection of clients.
newtype DisplayClients = DisplayClients (NM.NatMap DisplayClient)

newDisplayClients :: STM DisplayClients
newDisplayClients = DisplayClients <$> NM.newNatMap

getClients :: DisplayClients -> STM [DisplayClient]
getClients (DisplayClients x) = NM.elems x

addClient :: DisplayClients -> DisplayClient -> STM ()
addClient (DisplayClients x) = void . NM.add x

delClient :: DisplayClients -> DisplayClient -> STM ()
delClient (DisplayClients x) client = NM.nmDelete x (\o -> o.endpoint == client.endpoint)

newtype ClientsData a = ClientsData (TVar (Map Endpoint (TVar a)))

newClientsData :: STM (ClientsData a)
newClientsData = ClientsData <$> newTVar mempty

getClientsData :: ClientsData a -> STM [a]
getClientsData (ClientsData tv) = traverse readTVar =<< (Map.elems <$> readTVar tv)

addClientsData :: ClientsData a -> DisplayClient -> a -> STM ()
addClientsData (ClientsData tv) client value = do
    tValue <- newTVar value
    modifyTVar' tv $ Map.insert client.endpoint tValue

delClientsData :: ClientsData a -> DisplayClient -> STM ()
delClientsData (ClientsData tv) client = modifyTVar' tv $ Map.delete client.endpoint

lookupClientData :: ClientsData a -> DisplayClient -> STM (Maybe (TVar a))
lookupClientData (ClientsData tv) client = Map.lookup client.endpoint <$> readTVar tv

withClientsData :: ClientsData a -> DisplayClient -> (TVar a -> ProcessIO ()) -> ProcessIO ()
withClientsData cd client cb = do
    atomically (lookupClientData cd client) >>= \case
        Just v -> cb v
        Nothing -> logError "Unknown client" ["client" .= client]
