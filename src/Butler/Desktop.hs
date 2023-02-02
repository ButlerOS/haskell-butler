module Butler.Desktop (
    Handlers,
    Desktop (..),
    newDesktop,
    newDesktopIO,
    startDesktop,
    desktopHandler,
    newHandler,
    handleClientEvents,
    lookupDataClient,

    -- * app
    addApp,

    -- * messages
    broadcastDraw,
    broadcastHtmlT,
    broadcastDesktopMessage,
    broadcastRawDesktopMessage,
) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

import Butler.App
import Butler.Clock
import Butler.Display
import Butler.Frame
import Butler.GUI
import Butler.Logger
import Butler.Memory
import Butler.NatMap qualified as NM
import Butler.Prelude
import Butler.Processor
import Butler.Session
import Butler.SoundBlaster
import Butler.WebSocket
import Butler.Window

data Desktop = Desktop
    { env :: ProcessEnv
    , display :: Display
    , workspace :: Workspace
    , wm :: WindowManager
    , apps :: TVar (Map Pid GuiApp)
    , channels :: TVar (Map ChannelName (DisplayClient -> ProcessIO ()))
    -- ^ dedicated websocket, e.g. for novnc
    , handlers :: Handlers
    -- ^ data channel, e.g. for window manager and xterm
    , clients :: DisplayClients
    -- ^ data clients
    , tabClients :: TVar (Map (SessionID, TabID) DisplayClient)
    -- ^ data clients indexed by tab, to lookup from htmx client
    , hclients :: DisplayClients
    -- ^ htmx clients
    , soundCard :: SoundCard
    , desktopEvents :: BroadcastChan DisplayEvent
    }

newDesktop :: ProcessEnv -> Display -> Workspace -> WindowManager -> STM Desktop
newDesktop processEnv display ws wm = do
    Desktop processEnv display ws wm
        <$> newTVar mempty
        <*> newTVar mempty
        <*> NM.newNatMap
        <*> newDisplayClients
        <*> newTVar mempty
        <*> newDisplayClients
        <*> newSoundCard
        <*> newBroadcastChan

newDesktopIO :: Display -> Workspace -> ProcessIO Desktop
newDesktopIO display ws = do
    processEnv <- ask
    wm <- newWindowManager
    atomically (newDesktop processEnv display ws wm)

lookupDataClient :: Desktop -> DisplayClient -> STM (Maybe DisplayClient)
lookupDataClient desktop client = Map.lookup (client.session.sessionID, client.tabID) <$> readTVar desktop.tabClients

deskApp :: (WinID -> HtmlT STM ()) -> App
deskApp draw =
    App
        { name = "welcome"
        , description = mempty
        , tags = mempty
        , triggers = mempty
        , size = Nothing
        , start = \_ wid -> pure $ AppInstance (pureDraw $ draw wid) emptyDraw $ \_ev -> do
            forever do
                liftIO $ threadDelay maxBound
        }

startDesktop :: MVar Desktop -> (Desktop -> AppSet) -> (Desktop -> ProcessIO ()) -> Display -> Workspace -> ProcessIO ()
startDesktop desktopMVar mkAppSet xinit display name = do
    desktop <- newDesktopIO display name
    let appSet = mkAppSet desktop

    apps <- atomically $ readMemoryVar desktop.wm.apps

    chan <- atomically $ newHandler desktop.handlers (const $ tryHandleWinEvent desktop)
    when (chan /= winChannel) (error $ "Default win channel is wrong: " <> show chan)
    achan <- atomically $ newHandler desktop.handlers (soundHandler desktop.soundCard)
    when (achan /= audioChannel) (error $ "Default audio channel is wrong: " <> show achan)

    let mkDeskApp draw = startApp (deskApp draw) desktop.hclients

    xinit desktop

    case Map.toList apps of
        [] -> do
            (wid, _) <- atomically $ newWindow desktop.wm.windows "Welcome"
            atomically . addWinApp desktop wid =<< mkDeskApp (welcomeWin appSet) wid
        xs -> forM_ xs $ \(wid, prog) -> do
            mApp <- case prog of
                "app-welcome" -> Just <$> mkDeskApp (welcomeWin appSet) wid
                "app-launcher" -> Just <$> mkDeskApp (menuWin appSet) wid
                _ -> launchApp appSet prog desktop.hclients wid
            case mApp of
                Just app -> atomically $ addApp desktop app
                Nothing -> logError "Couldn't start app" ["wid" .= wid, "prog" .= prog]

    putMVar desktopMVar desktop

    let updateStatus s = do
            broadcastHtmlT desktop $ statusHtml s
            sleep 5_000
            updateStatus (not s)
    _ <- updateStatus True
    error "update status crashed?"

broadcastHtmlT :: Desktop -> HtmlT STM () -> ProcessIO ()
broadcastHtmlT desktop = clientsHtmlT desktop.hclients

broadcastDraw :: Desktop -> (DisplayClient -> ProcessIO (HtmlT STM ())) -> ProcessIO ()
broadcastDraw desktop = clientsDraw desktop.hclients

-- | Register a new gui app without window, e.g. for tray only app.
addApp :: Desktop -> GuiApp -> STM ()
addApp desktop app = modifyTVar' desktop.apps (Map.insert app.process.pid app)

-- | Remove a registered gui app
deleteApp :: Desktop -> Pid -> STM ()
deleteApp desktop pid = do
    modifyTVar' desktop.apps (Map.delete pid)
    void $ stopProcess desktop.env.os.processor pid

-- | Register a new windowed application.
addWinApp :: Desktop -> WinID -> GuiApp -> STM ()
addWinApp desktop wid app = do
    addApp desktop app
    addWindowApp desktop.wm wid app

terminateWinApp :: Desktop -> WinID -> STM ()
terminateWinApp desktop wid = do
    guiAppM <- Map.lookup wid <$> readTVar desktop.wm.running
    case guiAppM of
        Just guiApp -> deleteApp desktop guiApp.process.pid
        Nothing -> pure ()

deleteWinApp :: Desktop -> WinID -> STM ()
deleteWinApp desktop wid = do
    terminateWinApp desktop wid
    delWindowApp desktop.wm wid

swapGuiApp :: Desktop -> WinID -> GuiApp -> STM ()
swapGuiApp desktop wid app = do
    terminateWinApp desktop wid
    addWinApp desktop wid app

sendDesktopMessage :: Desktop -> ByteString -> DisplayClient -> ProcessIO ()
sendDesktopMessage desktop buf client = do
    safeSend desktop.clients sendMessage client buf

broadcastRawDesktopMessage :: Desktop -> (DisplayClient -> Bool) -> ByteString -> ProcessIO ()
broadcastRawDesktopMessage desktop fpred bs = do
    -- logTrace "broadcast desktop" ["msg" .= BSLog msg]
    clients <- filter fpred <$> atomically (getClients desktop.clients)
    traverse_ (sendDesktopMessage desktop bs) clients

broadcastDesktopMessage :: Desktop -> (DisplayClient -> Bool) -> ChannelID -> ByteString -> ProcessIO ()
broadcastDesktopMessage desktop fpred chan bs = do
    broadcastRawDesktopMessage desktop fpred (encodeMessage chan bs)

_writeHtml :: MonadIO m => TVar Text -> Html () -> m ()
_writeHtml t h = atomically $ writeTVar t (from $ renderText h)

desktopHtml :: Windows -> [HtmlT STM ()] -> [HtmlT STM ()] -> [HtmlT STM ()] -> HtmlT STM ()
desktopHtml windows trayContents menuContents appContents = do
    div_ [id_ "display-root", class_ "flex flex-col min-h-full"] do
        script_ clientScript
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

welcomeWin :: Monad m => AppSet -> WinID -> HtmlT m ()
welcomeWin appSet wid = do
    with div_ [id_ (withWID wid "w"), class_ "grid grid-cols-1 divide-y"] do
        with div_ [class_ "p-2"] do
            "Welcome to "
            with span_ [class_ "font-bold"] "ButlerOS"
        div_ do
            "Press start!"
        with div_ [class_ "m-2 border border-gray-500 rounded-md"] do
            appSetHtml wid appSet

menuWin :: Monad m => AppSet -> WinID -> HtmlT m ()
menuWin appSet wid = do
    with div_ [id_ (withWID wid "w")] do
        appSetHtml wid appSet

statusHtml :: Monad m => Bool -> HtmlT m ()
statusHtml s =
    let cls = bool "bg-sky-400" "bg-sky-300" s
     in with
            span_
            [ id_ "display-pulse"
            , class_ $ "m-auto mr-1 h-3 w-3 center rounded-full opacity-75 cursor-pointer " <> cls
            , hxTrigger_ "click"
            , wsSend
            ]
            mempty

_trayHtml :: Monad m => HtmlT m () -> HtmlT m ()
_trayHtml body =
    with span_ [id_ "display-tray", class_ "flex h-full w-full align-center jusity-center"] do
        body

desktopHandler :: AppSet -> Desktop -> DisplayEvent -> ProcessIO ()
desktopHandler appSet desktop event = do
    -- update desktop state with display event
    case event of
        UserConnected chan client -> do
            atomically do
                when (chan == "data") do
                    addClient desktop.clients client
                    modifyTVar' desktop.tabClients (Map.insert (client.session.sessionID, client.tabID) client)
                when (chan == "htmx") do
                    addClient desktop.hclients client
                broadcast desktop.desktopEvents event

            handleNewUser chan client
        UserDisconnected chan client -> do
            atomically do
                when (chan == "data") do
                    delClient desktop.clients client
                    delSoundClient desktop.soundCard client
                    modifyTVar' desktop.tabClients (Map.delete (client.session.sessionID, client.tabID))
                when (chan == "htmx") do
                    delClient desktop.hclients client
                broadcast desktop.desktopEvents event
  where
    handleNewUser :: ChannelName -> DisplayClient -> ProcessIO ()
    handleNewUser channel client = case channel of
        "htmx" -> do
            apps <- Map.elems <$> readTVarIO desktop.apps
            menuContents <- traverse (\app -> app.drawMenu client) (reverse apps)
            trayContents <- traverse (\app -> app.drawTray client) (reverse apps)
            appContents <- traverse (\app -> app.draw client) (reverse apps)

            sendHtml client (desktopHtml desktop.wm.windows trayContents menuContents appContents)
            handleHtmxClient appSet desktop client
        "data" -> do
            spawnPingThread client

            spawnThread_ $ forever do
                dataMessage <- atomically (readTChan client.sendChannel)
                sendMessage client dataMessage

            forever do
                buf <- recvMessage client

                case decodeMessage buf of
                    Nothing -> logError "invalid data message" ["buf" .= BSLog buf]
                    Just (chan, xs) -> do
                        handlerM <- atomically $ NM.lookup desktop.handlers (from chan)
                        case handlerM of
                            Just handler -> handler buf chan client xs
                            Nothing -> logError "unknown chan" ["chan" .= chan, "data" .= BSLog buf]
        _ -> do
            channels <- readTVarIO desktop.channels
            case Map.lookup channel channels of
                Just cb -> cb client
                Nothing -> logError "unknown channel" ["channel" .= channel]

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

handleHtmxClient :: AppSet -> Desktop -> DisplayClient -> ProcessIO ()
handleHtmxClient appSet desktop client = do
    -- todo: play starting sound
    sleep 500

    sendTextMessage client $ encodeUtf8 $ from $ renderText do
        with div_ [id_ "reconnect_script"] do
            -- After connection, we switch the socket url to the reload endpoint,
            -- so that after disconnection, it will receive new instruction, e.g. reload the page.
            script_ "htmx.find('#display-ws').setAttribute('ws-connect', '/ws/htmx?reconnect=true')"

    -- wait for user events
    handleClientEvents client handleEvent
  where
    handleEvent :: TriggerName -> Value -> ProcessIO ()
    handleEvent "win-swap" value = do
        case (value ^? key "prog" . _String, value ^? key "win" . _Integer) of
            (Just (ProgramName -> appName), Just (WinID . unsafeFrom -> winId)) -> do
                mGuiApp <- asProcess desktop.env (launchApp appSet appName desktop.hclients winId)
                case mGuiApp of
                    Just guiApp -> swapWindow winId guiApp
                    Nothing -> logInfo "unknown win-swap prog" ["v" .= value]
            _ -> logInfo "unknown win-swap" ["v" .= value]
    handleEvent "wm-start" _value = do
        logInfo "start" []
        addWindow Nothing
    handleEvent trigger value =
        traverse_ triggerApp =<< readTVarIO desktop.apps
      where
        triggerApp app
            | trigger `Set.member` app.triggers = do
                atomically $ writePipe app.events (GuiEvent client trigger value)
            | otherwise = pure ()

    addWindow :: Maybe GuiApp -> ProcessIO ()
    addWindow content = do
        (wid, script) <- atomically do
            (winId, win) <- newWindow desktop.wm.windows "REPL"
            let script = renderWindow (winId, win)
            pure (winId, script)

        guiApp <- case content of
            Just guiApp -> pure guiApp
            Nothing -> startApp (deskApp $ menuWin appSet) desktop.hclients wid
        atomically $ addWinApp desktop wid guiApp

        broadcastDraw desktop $ \oclient -> do
            c <- guiApp.draw oclient
            pure $ with div_ [id_ "backstore", hxSwapOob_ "beforeend"] do
                c
                with (script_ script) [type_ "module"]

    swapWindow :: WinID -> GuiApp -> ProcessIO ()
    swapWindow winId guiApp = do
        atomically $ swapGuiApp desktop winId guiApp
        broadcastDraw desktop $ \oclient -> do
            guiApp.draw oclient
        broadcastTitle winId (processID guiApp.process)
        case guiApp.size of
            Just sizeTV -> do
                size <- readTVarIO sizeTV
                broadcastSize winId size
                void $ atomically $ updateWindow desktop.wm.windows winId (#size .~ size)
            Nothing -> pure ()

    broadcastSize :: WinID -> (Int, Int) -> ProcessIO ()
    broadcastSize wid (x, y) =
        let body = ["w" .= wid, "ev" .= ("resize" :: Text), "x" .= x, "y" .= y]
         in broadcastDesktopMessage desktop (const True) winChannel $ from $ encodeJSON $ object body

    broadcastTitle :: WinID -> Text -> ProcessIO ()
    broadcastTitle wid title =
        let body = ["w" .= wid, "ev" .= ("title" :: Text), "title" .= title]
         in broadcastDesktopMessage desktop (const True) winChannel $ from $ encodeJSON $ object body

tryHandleWinEvent :: Desktop -> ChannelID -> DisplayClient -> ByteString -> ProcessIO ()
tryHandleWinEvent desktop chan client buf = case decode' (from buf) of
    Just obj -> case (obj ^? key "ev" . _String, obj ^? key "w" . _Integer) of
        (Just winEvent, Just (WinID . unsafeFrom -> winId)) -> do
            handleWinEvent desktop chan client buf winEvent winId obj
        _ -> logError "invalid win event" ["buf" .= BSLog buf]
    Nothing -> logError "unknown win event" ["buf" .= BSLog buf]

handleWinEvent :: Desktop -> ChannelID -> DisplayClient -> ByteString -> Text -> WinID -> Value -> ProcessIO ()
handleWinEvent desktop chan client buf ev wid v = do
    doBroadcast <-
        case ( ev
             , unsafeFrom <$> (v ^? key "x" . _Integer)
             , unsafeFrom <$> (v ^? key "y" . _Integer)
             ) of
            ("move", Just x, Just y) -> do
                atomically $ updateWindow desktop.wm.windows wid (#position .~ (x, y))
            ("resize", Just x, Just y) -> do
                atomically $ updateWindow desktop.wm.windows wid (#size .~ (x, y))
            ("close", Nothing, Nothing) -> do
                atomically $ deleteWinApp desktop wid
                logInfo "Delete win" ["wid" .= wid]
                pure True
            ("focus", Nothing, Nothing) -> do
                pure True
            _ -> do
                logError "invalid win-event" ["v" .= v]
                pure False
    when doBroadcast do
        broadcastDesktopMessage desktop (\o -> o.endpoint /= client.endpoint) chan buf
