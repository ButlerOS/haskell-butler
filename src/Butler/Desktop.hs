module Butler.Desktop where

import Data.ByteString qualified as BS
import Data.Char (isAlphaNum)
import Data.IntMap.Strict qualified as IM
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text

import Butler.Clock
import Butler.Display
import Butler.GUI
import Butler.Logger
import Butler.Memory
import Butler.NatMap qualified as NM
import Butler.Prelude
import Butler.Processor
import Butler.Storage
import Butler.WebSocket
import Butler.Window

data Desktop = Desktop
    { processEnv :: ProcessEnv
    , workspace :: Workspace
    , apps :: TVar (Map Pid GuiApp)
    , channels :: TVar (Map ChannelName (DisplayClient -> ProcessIO ()))
    -- ^ dedicated websocket, e.g. for novnc
    , handlers :: NM.NatMap (Natural -> DesktopClient -> ByteString -> ProcessIO ())
    -- ^ data channel, e.g. for window manager and xterm
    , display :: Display
    , windows :: Windows
    , winApps :: TVar (Map WinID GuiApp)
    , appHistory :: MemoryVar AppHistory
    , clients :: NM.NatMap DesktopClient
    , clientsChannel :: TVar (Map ChannelName [DisplayClient])
    }

type AppHistory = Map WinID ProgramName

broadcastMessage :: HasCallStack => MonadIO m => Desktop -> ChannelName -> ByteString -> m ()
broadcastMessage desktop channel message = liftIO do
    xs <- Map.lookup channel <$> readTVarIO desktop.clientsChannel
    forM_ (fromMaybe (error "unknown chan") xs) $ \client -> clientMessage client message

broadcastMessageT :: HasCallStack => MonadIO m => Desktop -> HtmlT STM () -> m ()
broadcastMessageT desktop message = liftIO do
    body <- from <$> atomically (renderBST message)
    xs <- fromMaybe [] . Map.lookup "htmx" <$> readTVarIO desktop.clientsChannel
    forM_ xs $ \client -> clientMessage client body

broadcastDraw :: HasCallStack => Desktop -> (DisplayClient -> ProcessIO (HtmlT STM ())) -> ProcessIO ()
broadcastDraw desktop draw = do
    xs <- fromMaybe [] . Map.lookup "htmx" <$> readTVarIO desktop.clientsChannel
    forM_ xs $ \client -> do
        htmlT <- draw client
        body <- from <$> atomically (renderBST htmlT)
        clientMessage client body

newDesktop :: ProcessEnv -> Display -> Workspace -> Windows -> MemoryVar AppHistory -> STM Desktop
newDesktop processEnv display ws windows appHistory =
    Desktop processEnv ws
        <$> newTVar mempty
        <*> newTVar mempty
        <*> NM.newNatMap
        <*> pure display
        <*> pure windows
        <*> newTVar mempty
        <*> pure appHistory
        <*> NM.newNatMap
        <*> newTVar mempty

newHandler :: Desktop -> (Natural -> DesktopClient -> ByteString -> ProcessIO ()) -> STM Natural
newHandler desktop = NM.add desktop.handlers

addApp :: MonadIO m => Desktop -> GuiApp -> m ()
addApp desktop app = atomically $ modifyTVar' desktop.apps (Map.insert app.process.pid app)

addWinApp :: MonadIO m => Desktop -> WinID -> GuiApp -> m ()
addWinApp desktop winID app = do
    addApp desktop app
    atomically do
        modifyTVar' desktop.winApps (Map.insert winID app)
        modifyMemoryVar desktop.appHistory (Map.insert winID app.process.program)
        void $ updateWindow desktop.windows winID (#title .~ processID app.process)

deleteApp :: Desktop -> Pid -> ProcessIO ()
deleteApp desktop pid = do
    os <- asks os
    atomically do
        modifyTVar' desktop.apps (Map.delete pid)
        void $ stopProcess os.processor pid

swapGuiApp :: Desktop -> WinID -> GuiApp -> ProcessIO ()
swapGuiApp desktop winID app = do
    guiAppM <- Map.lookup winID <$> readTVarIO desktop.winApps
    case guiAppM of
        Just guiApp -> deleteApp desktop guiApp.process.pid
        Nothing -> pure ()
    addWinApp desktop winID app

dataMessage :: Natural -> ByteString -> ByteString
dataMessage chan bs = case tryFrom chan of
    Right b8 -> BS.cons b8 bs
    Left e -> error $ "Run out of channel: " <> show e

sendDesktopMessage :: Desktop -> ByteString -> (Natural, DisplayClient) -> ProcessIO ()
sendDesktopMessage desktop buf (idx, client) = do
    v <- try (sendMessage client buf)
    case v of
        Left (_ :: SomeException) -> do
            logError "dclient down" ["client" .= client]
            atomically $ NM.delete desktop.clients idx
        Right () -> pure ()

broadcastDesktopMessage :: Desktop -> (DesktopClient -> Bool) -> Natural -> ByteString -> ProcessIO ()
broadcastDesktopMessage desktop fpred chan bs = do
    let msg = dataMessage chan bs
    -- logTrace "broadcast desktop" ["msg" .= BSLog msg]
    xs <- filter (fpred . snd) <$> atomically (NM.elemsIndex desktop.clients)
    traverse_ (sendDesktopMessage desktop msg . fmap (.client)) xs

deleteGuiApp :: Desktop -> WinID -> ProcessIO ()
deleteGuiApp desktop winID = do
    guiAppM <- Map.lookup winID <$> readTVarIO desktop.winApps
    case guiAppM of
        Just guiApp -> do
            deleteApp desktop guiApp.process.pid
        Nothing -> logError "Unknown win" ["wid" .= winID]
    atomically do
        deleteWindow desktop.windows winID
        modifyTVar' desktop.winApps (Map.delete winID)
        modifyMemoryVar desktop.appHistory (Map.delete winID)
    logInfo "Delete win" ["wid" .= winID]

writeHtml :: MonadIO m => TVar Text -> Html () -> m ()
writeHtml t h = atomically $ writeTVar t (from $ renderText h)

controlSocket :: Text
controlSocket =
    [raw|
// Helpers
globalThis.decodeJSON = buf => JSON.parse(new TextDecoder().decode(buf));
globalThis.encodeJSON = obj => (new TextEncoder()).encode(JSON.stringify(obj));
globalThis.encodeDataMessage = (chan, obj) => {
  let dataBuf = encodeJSON(obj)
  let buf = new Uint8Array(1 + dataBuf.length);
  buf[0] = chan;
  buf.set(new Uint8Array(dataBuf), 1)
  return buf
}

globalThis.windows = {}
globalThis.onWindowResize = {}

globalThis.butlerDataSocket = new WebSocket(wsUrl("data"));
globalThis.butlerDataHandlers = {
  // 0 is getWindowSize()
  0: (() => butlerDataSocket.send(JSON.stringify({w: window.innerWidth, h: window.innerHeight})))
  // 1 is window manager
}
butlerDataSocket.binaryType = 'arraybuffer';
globalThis.debounceData = (delay, handler) => {
  let timer;
  let lastEv;
  return (...ev) => {
    lastEv = ev
    if (timer === undefined) {
      timer = setTimeout(() => {
        msg = handler.apply(null, lastEv)
        if (msg) {
          butlerDataSocket.send(msg.buffer)
        }
        clearTimeout(timer);
        timer = undefined;
      }, delay);
    }
  };
}
butlerDataSocket.onerror = e => {
  console.log("data skt error", e)
}
butlerDataSocket.onmessage = event => {
  const buf = new Uint8Array(event.data)
  butlerDataHandlers[buf[0]](buf.slice(1))
}
|]

desktopHtml :: Windows -> [HtmlT STM ()] -> [HtmlT STM ()] -> [HtmlT STM ()] -> HtmlT STM ()
desktopHtml windows trayContents menuContents appContents = do
    div_ [id_ "display-root", class_ "flex flex-col min-h-full"] do
        script_ controlSocket
        -- [style_ " grid-template-columns: repeat(auto-fill, minmax(600px, 1fr));", class_ "grid"]
        with div_ [id_ "win-root", class_ "flex grow min-h-full"] do
            mempty

        -- bottom bar
        with nav_ [id_ "display-menu", class_ "h-9 flex-none bg-slate-700 p-1 shadow w-full flex text-white shrink sticky bottom-0 z-50"] do
            with' div_ "grow" do
                with span_ [class_ "font-semibold mr-5", hxTrigger_ "click", id_ "wm-start", wsSend] ">>= start"
                sequence_ menuContents
            with' div_ "display-bar-right" do
                with span_ [id_ "display-tray", class_ "flex h-full w-full align-center jusity-center"] do
                    sequence_ trayContents
                    statusHtml False

        with div_ [id_ "reconnect_script"] mempty

        with div_ [id_ "backstore"] do
            sequence_ appContents
            renderWindows windows

welcomeWin :: Monad m => WinID -> HtmlT m ()
welcomeWin (WinID winId) = do
    with div_ [id_ ("w-" <> showT winId), class_ "grid grid-cols-1 divide-y"] do
        with div_ [class_ "p-2"] do
            "Welcome to "
            with span_ [class_ "font-bold"] "ButlerOS"
        div_ do
            "Press start!"
        with div_ [class_ "m-2 border border-gray-500 rounded-md"] do
            menuHtml (WinID winId)

menuHtml :: Monad m => WinID -> HtmlT m ()
menuHtml (WinID winId) = do
    with ul_ [class_ "list-disc"] do
        mkLauncher "clock"
        mkLauncher "minesweeper"
        mkLauncher "ps"
        mkLauncher "log-viewer"
        mkLauncher "term"
        mkLauncher "vnc"
  where
    mkLauncher :: Text -> _
    mkLauncher prog =
        with li_ [id_ "win-swap", encodeVal ["win" .= winId, "prog" .= ("app-" <> prog)], class_ "cursor-pointer", wsSend, hxTrigger_ "click"] (toHtml prog)

menuWin :: Monad m => WinID -> HtmlT m ()
menuWin (WinID winId) =
    with div_ [id_ ("w-" <> showT winId)] do
        menuHtml (WinID winId)

statusHtml :: Monad m => Bool -> HtmlT m ()
statusHtml s =
    let cls = bool "bg-sky-400" "bg-sky-300" s
     in with
            span_
            [ id_ "display-pulse"
            , class_ $ "m-auto mr-1 h-3 w-3 center rounded-full opacity-75 " <> cls
            , hxTrigger_ "click"
            , wsSend
            ]
            mempty

trayHtml :: Monad m => HtmlT m () -> HtmlT m ()
trayHtml body =
    with span_ [id_ "display-tray", class_ "flex h-full w-full align-center jusity-center"] do
        body

data DesktopClient = DesktopClient
    { client :: DisplayClient
    , resolution :: TVar (Int, Int)
    , cursor :: TVar (Maybe (Int, Int))
    }

newWorkspaces :: MonadIO m => m (MVar (Map Workspace Desktop))
newWorkspaces = newMVar mempty

welcomeHtml :: Map Workspace Desktop -> HtmlT STM ()
welcomeHtml wss =
    with div_ [id_ "display-root"] do
        script_ "htmx.find('#display-ws').setAttribute('ws-connect', '/ws/htmx?reconnect=true')"
        let btn = "rounded-none px-4 py-2 font-semibold text-sm bg-sky-500 text-white rounded-none shadow-sm"
        splashHtml do
            with div_ [class_ "flex"] do
                with div_ [class_ "basis-1/3 flex flex-col place-content-center pr-3"] do
                    with div_ [class_ "items-center justify-center whitespace-nowrap"] do
                        "Welcome to "
                        with span_ [class_ "font-bold"] "ButlerOS"
                    with div_ [class_ "items-center justify-center grid"] do
                        img_ [src_ "/xstatic/favicon.ico"]
                with div_ [class_ "border border-2 mr-3"] mempty
                with div_ [class_ "flex-grow"] do
                    with div_ [class_ "flex flex-col place-items-center"] do
                        with div_ [class_ "grid auto-cols-auto w-96"] do
                            forM_ (Map.toList wss) $ \(ws, desktop) -> do
                                with
                                    div_
                                    [ class_ "p-2 mt-4 bg-stone-300 rounded cursor-pointer relative"
                                    ]
                                    do
                                        with div_ [class_ "flex justify-end absolute bottom-0 right-0"] do
                                            with
                                                i_
                                                [ id_ "delete-ws"
                                                , encodeVal [("ws", toJSON ws)]
                                                , class_ "px-4 py-2 ri-focus-3-fill text-red-500"
                                                , hxTrigger_ "click"
                                                , wsSend
                                                ]
                                                mempty
                                            with
                                                button_
                                                [ hxTrigger_ "click"
                                                , wsSend
                                                , id_ "enter-ws"
                                                , encodeVal [("ws", toJSON ws)]
                                                , class_ btn
                                                ]
                                                "enter"
                                        table_ do
                                            let attr :: Text -> Text -> HtmlT STM ()
                                                attr (k :: Text) (v :: Text) =
                                                    tr_ do
                                                        with td_ [class_ "text-right pr-1"] do
                                                            toHtml k
                                                            ":"
                                                        with td_ [class_ "font-medium"] do
                                                            toHtml v
                                            attr "name" (showT ws)
                                            client <- lift (NM.nmLength desktop.clients)
                                            attr "clients" (showT client)
                                            wins <- IM.size . (.windows) <$> lift (readMemoryVar desktop.windows)
                                            attr "wins" (showT wins)
                        with form_ [wsSend, hxTrigger_ "submit", class_ "mt-4 p-2 flex flex-col bg-stone-300 rounded w-96 relative", id_ "new-ws"] do
                            with (input_ mempty) [name_ "ws", type_ "text", placeholder_ "Workspace name"]
                            with select_ [name_ "flavor"] do
                                option_ "localhost"
                                option_ "quay.io/org/toolbox"
                            with (input_ mempty) [type_ "submit", class_ btn, value_ "create"]

welcomeDesktop :: MVar (Map Workspace Desktop) -> DisplayEvent -> ProcessIO ()
welcomeDesktop workspaces = \case
    UserConnected "htmx" client -> do
        spawnPingThread client
        clientMessageT client (with div_ [id_ "display-root"] (splashHtml "Loading..."))
        sleep 500
        wss <- readMVar workspaces
        clientMessageT client (welcomeHtml wss)
        handleClientEvents client $ handleWelcomeEvents workspaces client
    _ -> pure ()

desktopProgram :: (Desktop -> ProgramName -> WinID -> ProcessIO (Maybe GuiApp)) -> (Desktop -> ProcessIO ()) -> ProcessIO Void
desktopProgram appLauncher xinit = startDisplay 8080 \display -> do
    displayProcess <- getSelfProcess

    workspaces <- newWorkspaces
    let getDesktop :: Workspace -> ProcessIO Desktop
        getDesktop name = modifyMVar workspaces $ \wss -> case Map.lookup name wss of
            Just n -> pure (wss, n)
            Nothing -> do
                desktopMVar <- newEmptyMVar
                let desktopID = "desktop-" <> from name
                os <- asks os
                desktopStorage <- scopeStorage os.storage (from desktopID)
                void $ spawnProcess (ProgramName desktopID) $ local (#os . #storage .~ desktopStorage) do
                    processEnv <- ask

                    windows <- snd <$> newProcessMemory "wins.bin" (pure newWindows)
                    (apps, appHistory) <- newProcessMemory "apps.bin" (pure mempty)

                    desktop <- atomically (newDesktop processEnv display name windows appHistory)
                    void $ atomically $ newHandler desktop (tryHandleWinEvent desktop)

                    let mkWelcome winID = newGuiApp "welcome" Nothing (const . pure $ welcomeWin winID) [] (const $ pure ())

                    xinit desktop

                    case Map.toList apps of
                        [] -> do
                            (winID, _) <- atomically $ newWindow desktop.windows "Welcome"
                            addWinApp desktop winID =<< mkWelcome winID
                        xs -> forM_ xs $ \(winID, prog) -> do
                            appM <- case prog of
                                "app-welcome" -> Just <$> mkWelcome winID
                                "app-launcher" -> Just <$> newGuiApp (ProgramName "launcher") Nothing (const . pure $ menuWin winID) [] (const $ pure ())
                                _ -> appLauncher desktop prog winID
                            case appM of
                                Just app -> addWinApp desktop winID app
                                Nothing -> logError "Couldn't start app" ["wid" .= winID, "prog" .= prog]

                    putMVar desktopMVar desktop
                    _ <- updateStatus desktop True

                    error "update status crashed?"
                desktop <- takeMVar desktopMVar
                pure (Map.insert name desktop wss, desktop)

    _rootProcess <- spawnProcess "welcome-desktop" do
        void $ awaitProcess displayProcess
    rootOS <- ask

    void $ getDesktop "default"

    pure \case
        Workspace "" -> pure (rootOS, welcomeDesktop workspaces)
        name -> do
            desktop <- getDesktop name
            let handl event = do
                    -- update desktop state with display event
                    let addClient client = \case
                            Nothing -> Just [client]
                            Just xs -> Just (client : xs)
                        delClient endpoint = \case
                            Nothing -> Nothing
                            Just xs -> Just (filter (\c -> c.endpoint /= endpoint) xs)
                    case event of
                        UserConnected chan client ->
                            atomically $ modifyTVar' desktop.clientsChannel $ Map.alter (addClient client) chan
                        UserDisconnected chan endpoint -> do
                            atomically do
                                modifyTVar' desktop.clientsChannel $ Map.alter (delClient endpoint) chan
                                when (chan == "data") do
                                    NM.nmDelete desktop.clients (\dclient -> dclient.client.endpoint == endpoint)
                    case event of
                        UserConnected "htmx" client -> do
                            apps <- Map.elems <$> readTVarIO desktop.apps
                            menuContents <- traverse (\app -> app.drawMenu client) (reverse apps)
                            trayContents <- traverse (\app -> app.drawTray client) (reverse apps)
                            appContents <- traverse (\app -> app.draw client) (reverse apps)

                            clientMessageT client (desktopHtml desktop.windows trayContents menuContents appContents)
                            handleHtmxClient appLauncher desktop client
                        UserConnected "data" client -> do
                            spawnPingThread client
                            dim <- queryClient client "\x00"
                            dclient <- case (dim ^? key "w" . _Integer, dim ^? key "h" . _Integer) of
                                (Just (unsafeFrom -> w), Just (unsafeFrom -> h)) -> do
                                    DesktopClient client <$> newTVarIO (w, h) <*> newTVarIO Nothing
                                _ -> do
                                    logError "invalid dim" ["buf" .= BSLog dim]
                                    error "bad client"

                            atomically $ void $ NM.add desktop.clients dclient

                            forever do
                                buf <- recvMessage client

                                case BS.uncons buf of
                                    Nothing -> logError "empty data message" []
                                    Just (from -> chan, xs) -> do
                                        handlerM <- atomically $ NM.lookup desktop.handlers chan
                                        case handlerM of
                                            Just handler -> handler chan dclient xs
                                            Nothing -> logError "unknown chan" ["chan" .= chan, "data" .= BSLog buf]
                        UserConnected channel client -> do
                            channels <- readTVarIO desktop.channels
                            case Map.lookup channel channels of
                                Just cb -> cb client
                                Nothing -> logError "unknown channel" ["channel" .= channel]
                        _ -> pure ()

            pure (desktop.processEnv, handl)
  where
    queryClient :: DisplayClient -> ByteString -> ProcessIO ByteString
    queryClient client buf = do
        sendMessage client buf
        fromMaybe (error "No reply") <$> timeout 500_000 (recvMessage client)

    updateStatus :: Desktop -> Bool -> ProcessIO Void
    updateStatus desktop s = do
        broadcastMessageT desktop $ statusHtml s
        sleep 5_000
        updateStatus desktop (not s)

handleWelcomeEvents :: MVar (Map Workspace Desktop) -> DisplayClient -> TriggerName -> Value -> ProcessIO ()
handleWelcomeEvents workspaces client trigger ev = case ev ^? key "ws" . _String of
    Just wsTxt -> do
        let ws = Workspace wsTxt
        logInfo "got welcome event" ["ws" .= ws, "trigger" .= trigger]
        case trigger of
            "delete-ws" -> modifyMVar_ workspaces $ \wss -> case Map.lookup ws wss of
                Just desktop -> do
                    void $ killProcess desktop.processEnv.process.pid
                    pure (Map.delete ws wss)
                Nothing -> do
                    logError "unknown ws" ["ws" .= ws]
                    pure wss
            "enter-ws" -> do
                clientMessageT client do
                    with div_ [id_ "display-root"] do
                        script_ $ "window.location.pathname = \"/" <> wsTxt <> "\""
            "new-ws" -> do
                let cleanWS = Text.takeWhile (\c -> isAlphaNum c || c == '-') wsTxt
                case cleanWS of
                    "" -> logError "invalid ws" ["ws" .= cleanWS]
                    _ -> do
                        clientMessageT client do
                            with div_ [id_ "display-root"] do
                                script_ $ "window.location.pathname = \"/" <> cleanWS <> "\""
            _ -> logError "unknown welcome event" ["ev" .= ev]
    Nothing -> logError "missing ws" ["ev" .= ev]

handleClientEvents :: DisplayClient -> (TriggerName -> Value -> ProcessIO ()) -> ProcessIO ()
handleClientEvents client cb = do
    forever do
        buf <- recvMessage client
        case decode' (from buf) of
            Just v -> do
                case v ^? key "HEADERS" . key "HX-Trigger" . _String of
                    Just trigger -> cb (TriggerName trigger) v
                    Nothing -> logError "event without trigger" ["event" .= v]
            Nothing -> logError "Received unknown websocket data" ["buf" .= BSLog buf]

handleHtmxClient :: _ -> Desktop -> DisplayClient -> ProcessIO ()
handleHtmxClient appLauncher desktop client = do
    -- todo: play starting sound
    sleep 500

    clientMessage client $ from $ renderText do
        with div_ [id_ "reconnect_script"] do
            -- After connection, we switch the socket url to the reload endpoint,
            -- so that after disconnection, it will receive new instruction, e.g. reload the page.
            script_ "htmx.find('#display-ws').setAttribute('ws-connect', '/ws/htmx?reconnect=true')"

    -- wait for user events
    handleClientEvents client (handleEvent client)
  where
    handleEvent :: DisplayClient -> TriggerName -> Value -> ProcessIO ()
    handleEvent _client "win-swap" value = do
        case (value ^? key "prog" . _String, value ^? key "win" . _Integer) of
            (Just (ProgramName -> appName), Just (WinID . unsafeFrom -> winId)) -> do
                guiAppM <- asProcess desktop.processEnv (appLauncher desktop appName winId)
                case guiAppM of
                    Just guiApp -> swapWindow winId guiApp
                    Nothing -> logInfo "unknown win-swap prog" ["v" .= value]
            _ -> logInfo "unknown win-swap" ["v" .= value]
    handleEvent _client "wm-start" _value = do
        logInfo "start" []
        addWindow Nothing
    handleEvent _client trigger value =
        traverse_ triggerApp =<< readTVarIO desktop.apps
      where
        triggerApp app
            | trigger `Set.member` app.triggers = do
                atomically $ writePipe app.events (GuiEvent client trigger value)
            | otherwise = pure ()

    addWindow :: Maybe GuiApp -> ProcessIO ()
    addWindow content = do
        (winId, script) <- atomically do
            (winId, win) <- newWindow desktop.windows "REPL"
            let script = renderWindow (winId, win)
            pure (winId, script)

        guiApp <- case content of
            Just guiApp -> pure guiApp
            Nothing -> newGuiApp (ProgramName "launcher") Nothing (const . pure $ menuWin winId) [] (const $ pure ())

        addWinApp desktop winId guiApp

        broadcastDraw desktop $ \oclient -> do
            c <- guiApp.draw oclient
            pure $ with div_ [id_ "backstore", hxSwapOob_ "beforeend"] do
                c
                with (script_ script) [type_ "module"]

    swapWindow :: WinID -> GuiApp -> ProcessIO ()
    swapWindow winId guiApp = do
        swapGuiApp desktop winId guiApp
        broadcastDraw desktop $ \oclient -> do
            guiApp.draw oclient
        broadcastTitle winId (processID guiApp.process)
        case guiApp.size of
            Just sizeTV -> do
                size <- readTVarIO sizeTV
                broadcastSize winId size
                void $ atomically $ updateWindow desktop.windows winId (#size .~ size)
            Nothing -> pure ()

    broadcastSize :: WinID -> (Int, Int) -> ProcessIO ()
    broadcastSize wid (x, y) =
        let body = ["w" .= wid, "ev" .= ("resize" :: Text), "x" .= x, "y" .= y]
         in broadcastDesktopMessage desktop (const True) 1 $ from $ encodeJSON $ object body

    broadcastTitle :: WinID -> Text -> ProcessIO ()
    broadcastTitle wid title =
        let body = ["w" .= wid, "ev" .= ("title" :: Text), "title" .= title]
         in broadcastDesktopMessage desktop (const True) 1 $ from $ encodeJSON $ object body

tryHandleWinEvent :: Desktop -> Natural -> DesktopClient -> ByteString -> ProcessIO ()
tryHandleWinEvent desktop chan dclient buf = case decode' (from buf) of
    Just obj -> case (obj ^? key "ev" . _String, obj ^? key "w" . _Integer) of
        (Just winEvent, Just (WinID . unsafeFrom -> winId)) -> do
            handleWinEvent desktop chan dclient.client buf winEvent winId obj
        _ -> logError "invalid win event" ["buf" .= BSLog buf]
    Nothing -> logError "unknown win event" ["buf" .= BSLog buf]

handleWinEvent :: Desktop -> Natural -> DisplayClient -> ByteString -> Text -> WinID -> Value -> ProcessIO ()
handleWinEvent desktop chan client buf ev wid v = do
    doBroadcast <-
        case ( ev
             , unsafeFrom <$> (v ^? key "x" . _Integer)
             , unsafeFrom <$> (v ^? key "y" . _Integer)
             ) of
            ("move", Just x, Just y) -> do
                atomically $ updateWindow desktop.windows wid (#position .~ (x, y))
            ("resize", Just x, Just y) -> do
                atomically $ updateWindow desktop.windows wid (#size .~ (x, y))
            ("close", Nothing, Nothing) -> do
                deleteGuiApp desktop wid
                pure True
            ("focus", Nothing, Nothing) -> do
                pure True
            _ -> do
                logError "invalid win-event" ["v" .= v]
                pure False
    when doBroadcast do
        broadcastDesktopMessage desktop (\o -> o.client.endpoint /= client.endpoint) chan buf
