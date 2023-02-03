module Butler.DisplayClient where

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.List qualified (find)
import Data.Text qualified as Text
import Lucid
import Network.WebSockets qualified as WS

import Butler.Clock
import Butler.NatMap qualified as NM
import Butler.OS
import Butler.Prelude
import Butler.Session

data DisplayClient = DisplayClient
    { conn :: WS.Connection
    , endpoint :: Endpoint
    , process :: Process
    , session :: Session
    , tabID :: TabID
    , recv :: TVar Word64
    , send :: TVar Word64
    , sendChannel :: TChan WS.DataMessage
    }

newClient :: WS.Connection -> Endpoint -> Process -> Session -> TabID -> STM DisplayClient
newClient c endpoint p s t = DisplayClient c endpoint p s t <$> newTVar 0 <*> newTVar 0 <*> newTChan

spawnPingThread :: DisplayClient -> ProcessIO ()
spawnPingThread client = spawnThread_ $ forever do
    sleep 30_000
    liftIO (WS.sendPing client.conn ("ping" :: ByteString))

recvMessage :: MonadIO m => DisplayClient -> m ByteString
recvMessage client = do
    buf <- liftIO (WS.receiveData client.conn)
    atomically $ modifyTVar' client.recv (+ unsafeFrom (BS.length buf))
    pure buf

spawnSendThread :: DisplayClient -> ProcessIO ()
spawnSendThread client = spawnThread_ $ forever do
    dataMessage <- atomically (readTChan client.sendChannel)
    let messageLBS = case dataMessage of
            WS.Text lbs _ -> lbs
            WS.Binary lbs -> lbs
    atomically $ modifyTVar' client.send (+ unsafeFrom (LBS.length messageLBS))
    liftIO $ WS.sendDataMessage client.conn dataMessage

writeBinaryMessage :: MonadIO m => DisplayClient -> LByteString -> m ()
writeBinaryMessage client t = liftIO do
    atomically $ modifyTVar' client.send (+ unsafeFrom (LBS.length t))
    WS.sendBinaryData client.conn t

writeTextMessage :: MonadIO m => DisplayClient -> Text -> m ()
writeTextMessage client txt = liftIO do
    atomically $ modifyTVar' client.send (+ unsafeFrom (Text.length txt))
    WS.sendTextData client.conn txt

sendHtml :: DisplayClient -> HtmlT STM () -> STM ()
sendHtml client htmlT = do
    body <- renderBST htmlT
    writeTChan client.sendChannel (WS.Text body Nothing)

sendsHtml :: DisplayClients -> HtmlT STM () -> ProcessIO ()
sendsHtml clients htmlT = do
    body <- atomically $ renderBST htmlT
    xs <- atomically $ getClients clients
    forM_ xs \client -> atomically (writeTChan client.sendChannel (WS.Text body Nothing))

sendBinary :: DisplayClient -> LByteString -> STM ()
sendBinary client buf = writeTChan client.sendChannel (WS.Binary buf)

sendsBinary :: DisplayClients -> LByteString -> ProcessIO ()
sendsBinary clients buf = do
    xs <- atomically $ getClients clients
    forM_ xs $ \client -> atomically $ sendBinary client buf

sendsBinaryButSelf :: DisplayClient -> DisplayClients -> LByteString -> ProcessIO ()
sendsBinaryButSelf self clients buf = do
    xs <- atomically $ getClients clients
    forM_ xs $ \client ->
        when (client.endpoint /= self.endpoint) do
            atomically $ writeTChan client.sendChannel (WS.Binary buf)

clientsHtmlT :: DisplayClients -> HtmlT STM () -> ProcessIO ()
clientsHtmlT clients htmlT = do
    body <- atomically $ renderBST htmlT
    xs <- atomically $ getClients clients
    forM_ xs $ \client -> atomically $ writeTChan client.sendChannel (WS.Text body Nothing)

clientsDraw :: DisplayClients -> (DisplayClient -> ProcessIO (HtmlT STM ())) -> ProcessIO ()
clientsDraw clients draw = do
    xs <- atomically (getClients clients)
    forM_ xs $ \client -> do
        htmlT <- draw client
        body <- atomically $ renderBST htmlT
        atomically $ writeTChan client.sendChannel (WS.Text body Nothing)

newtype Endpoint = Endpoint Text
    deriving newtype (Eq, Ord, Show, IsString)
    deriving (FromJSON, ToJSON, ToHtml) via Text

instance ToJSON DisplayClient where
    toJSON dc = object ["endpoint" .= dc.endpoint, "session" .= dc.session, "tab" .= dc.tabID]

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

lookupTabClient :: DisplayClients -> DisplayClient -> STM (Maybe DisplayClient)
lookupTabClient clients client = Data.List.find sameTab <$> getClients clients
  where
    sameTab oclient = oclient.session.sessionID == client.session.sessionID && oclient.tabID == client.tabID
