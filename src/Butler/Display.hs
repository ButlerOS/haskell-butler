-- | This module contains the logic to enable user access through HTTP.
module Butler.Display (
    Display (..),
    DisplayAddr (..),
    AuthApplication (..),
    OnClient,
    startDisplay,
    getClient,
    DisplayEvent (..),
    JwkStorage (..),
    DisplayApplication (..),
    module Butler.Display.Client,

    -- * Application environment
    serveApps,
    serveDashboardApps,
) where

import Codec.Serialise.Decoding (decodeBytes)
import Codec.Serialise.Encoding (encodeBytes)
import Crypto.JOSE.JWK qualified as JOSE
import Data.Aeson (decodeStrict')
import Data.Attoparsec.ByteString.Char8 qualified as P
import Data.List (find)
import Data.Map.Strict qualified as Map
import Lucid
import Lucid.Htmx
import Network.HTTP.Types.Status qualified as HTTP
import Network.Socket
import Network.Wai qualified
import Network.WebSockets qualified as WS
import Servant

import Butler.App
import Butler.Core
import Butler.Core.Clock
import Butler.Core.Logger
import Butler.Core.Network
import Butler.Core.Process
import Butler.Core.Storage
import Butler.Display.Client
import Butler.Display.GUI
import Butler.Display.Session
import Butler.Display.WebSocket
import Butler.Prelude
import System.Posix.Env.ByteString (getEnv)
import XStatic.Butler

type OnClient = (Session -> Workspace -> ProcessIO (ProcessEnv, DisplayEvent -> ProcessIO ()))

newDisplay :: Sessions -> STM Display
newDisplay sessions = Display sessions <$> newTVar mempty

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

connectRoute :: Display -> OnClient -> SockAddr -> Workspace -> ChannelName -> Session -> WS.Connection -> ProcessIO ()
connectRoute display onClient sockAddr workspaceM channel session connection = do
    let clientAddr = from $ show sockAddr
        ChannelName cn = channel
        progName = "client-" <> cn
        name = ProgramName $ progName <> "-" <> clientAddr
        endpoint = Endpoint clientAddr
    (processEnv, handler) <- onClient session workspaceM
    clientM <- newEmptyMVar
    clientProcess <- asProcess processEnv $ spawnProcess name do
        clientProcess <- getSelfProcess
        client <- atomically (newClient connection endpoint clientProcess session)
        putMVar clientM client
        -- Add the client to server state
        let ev = UserConnected channel client
        atomically do
            addDisplayClient display client
        handler ev

    client <- takeMVar clientM
    -- Wait for client completion
    res <- atomically $ await clientProcess.thread

    -- Remove the client from the server state
    let ev = UserDisconnected channel client
    atomically do
        removeClient display session.sessionID endpoint
    asProcess processEnv $ handler ev
    logInfo "Client quit" ["endpoint" .= endpoint, "reason" .= into @Text res]

    -- Say goodbye
    case res of
        Killed -> liftIO do
            -- TODO: check for SocketClosed exception here
            WS.sendTextData connection $ renderText do
                with div_ [id_ "display-wins", class_ "h-full w-full", hxSwapOob_ "afterbegin"] dcSplash
            WS.sendClose connection ("see you next time!" :: ByteString)
        _ -> pure ()

data AuthApplication = AuthApplication
    { app :: WaiApplication
    , getSession :: Maybe SessionID -> ProcessIO (Maybe Session)
    }

data DisplayAddr = DisplayAddr WebProtocol Port
    deriving (Eq)

defaultAddr :: DisplayAddr
defaultAddr = DisplayAddr (Https Nothing) 8085

{- | Parse listening addr
-- >>> displayAddrFromEnv "http:8080" @?= DisplayAddr Http 8080
-}
displayAddrFromEnv :: Maybe ByteString -> DisplayAddr
displayAddrFromEnv = \case
    Nothing -> defaultAddr
    Just env -> case P.parseOnly addrParser env of
        Left e -> error $ "Invalid addr: " <> e
        Right x -> x
  where
    addrParser :: P.Parser DisplayAddr
    addrParser =
        DisplayAddr
            <$> (Http <$ "http:" <|> Https Nothing <$ "https:")
            <*> P.decimal

startDisplay :: Maybe DisplayAddr -> [XStaticFile] -> (Sessions -> ProcessIO AuthApplication) -> (Display -> ProcessIO OnClient) -> ProcessIO Void
startDisplay mAddr xfiles mkAuthApp withDisplay = withSessions \sessions -> do
    DisplayAddr proto port <- case mAddr of
        Nothing -> displayAddrFromEnv <$> liftIO (getEnv "BUTLER_ADDR")
        Just addr -> pure addr
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

    webService xfiles glApp port proto

newtype DisplayApplication
    = DisplayApplication
        ( [XStaticFile] -> (Sessions -> ProcessIO AuthApplication)
        )

data SessionApps = SessionApps
    { env :: ProcessEnv
    , clients :: DisplayClients
    , shared :: AppSharedContext
    }

-- | Group apps per session.
newtype AllSessionApps = AllSessionApps (MVar (Map SessionID SessionApps))

startSessionApps :: AllSessionApps -> Display -> SessionID -> [App] -> ProcessIO SessionApps
startSessionApps (AllSessionApps mv) display sessionID apps =
    modifyMVar mv \allSessionApps -> case Map.lookup sessionID allSessionApps of
        Just sa -> pure (allSessionApps, sa)
        Nothing -> do
            clients <- atomically newDisplayClients
            -- Create a MVar so that the process can pass the created AppSharedContext
            mvShared <- newEmptyMVar
            -- Create a new process to manage a given session
            process <- doStartSessionApps clients mvShared
            -- Read the AppSharedContext
            shared <- takeMVar mvShared
            -- Create the SessionApps
            os <- asks os
            let sessionEnv = ProcessEnv os process
                sa = SessionApps sessionEnv clients shared
            pure (Map.insert sessionID sa allSessionApps, sa)
  where
    processName = ProgramName $ "sess-" <> into @Text sessionID
    storageAddr = StorageAddress "sess-" <> into sessionID

    doStartSessionApps clients mvShared = spawnProcess processName $ chroot storageAddr do
        -- Start the apps
        shared <- startApps apps display clients
        -- Pass the created AppSharedContext
        putMVar mvShared shared
        -- Wait until all clients disconnected
        waitForDisconnect clients
        -- Remove the sa
        modifyMVar_ mv (pure . Map.delete sessionID)

    waitForDisconnect clients = fix \loop -> do
        sleep 5_000
        atomically (getClients clients) >>= \case
            (_ : _) ->
                -- clients are still connected, keep on waiting.
                loop
            [] -> do
                -- clients are disconnected, wait 5 more seconds.
                sleep 5_000
                atomically (getClients clients) >>= \case
                    (_ : _) ->
                        -- a client connected back, likely a refresh, keep on waiting.
                        loop
                    [] ->
                        -- this is the end, exit the scope
                        pure ()

-- | Serve applications with one instance per client.
serveApps :: DisplayApplication -> [App] -> ProcessIO Void
serveApps (DisplayApplication mkAuth) apps = do
    void $
        waitProcess =<< superviseProcess "gui" do
            startDisplay Nothing xfiles (mkAuth xfiles) $ \display -> do
                allSessionApps <- AllSessionApps <$> newMVar mempty
                pure $ \session _ws -> do
                    sa <- startSessionApps allSessionApps display session.sessionID apps
                    pure (sa.env, staticClientHandler sa.clients sa.shared)
    error "Display exited?!"
  where
    xfiles = concatMap (.xfiles) apps <> defaultXFiles

-- | Serve applications with one instance for all clients.
serveDashboardApps :: DisplayApplication -> [App] -> ProcessIO Void
serveDashboardApps (DisplayApplication mkAuth) apps = do
    void $
        waitProcess =<< superviseProcess "gui" do
            startDisplay Nothing xfiles (mkAuth xfiles) $ \display -> do
                clients <- atomically newDisplayClients
                shared <- startApps apps display clients
                pure $ \_session _ws -> do
                    env <- ask
                    pure (env, staticClientHandler clients shared)
    error "Display exited?!"
  where
    xfiles = concatMap (.xfiles) apps <> defaultXFiles

staticClientHandler :: DisplayClients -> AppSharedContext -> DisplayEvent -> ProcessIO ()
staticClientHandler clients shared displayEvent = case displayEvent of
    UserConnected "htmx" client -> do
        spawnThread_ (pingThread client)
        spawnThread_ (sendThread client)
        atomically $ addClient clients client
        appInstances <- atomically (getApps shared.apps)
        atomically $ sendHtml client do
            with div_ [id_ "display-wins", class_ "flex"] do
                forM_ appInstances \appInstance -> do
                    with div_ [wid_ appInstance.wid "w"] mempty
        forM_ appInstances \appInstance -> writePipe appInstance.pipeAE (AppDisplay displayEvent)
        forever do
            dataMessage <- recvData client
            case eventFromMessage client dataMessage of
                Nothing -> logError "Unknown data" ["ev" .= LBSLog (into @LByteString dataMessage)]
                Just (wid, ae) -> case Map.lookup wid appInstances of
                    Just appInstance -> writePipe appInstance.pipeAE ae
                    Nothing -> logError "Unknown wid" ["wid" .= wid]
    UserDisconnected "htmx" client -> do
        logInfo "Client disconnected" ["client" .= client]
        atomically $ delClient clients client
        appInstances <- atomically (getApps shared.apps)
        forM_ appInstances \appInstance -> do
            writePipe appInstance.pipeAE (AppDisplay displayEvent)
    _ -> logError "Unknown event" ["ev" .= displayEvent]
