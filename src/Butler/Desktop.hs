module Butler.Desktop (
    Desktop (..),
    newDesktop,
    newDesktopIO,
    startDesktop,
    desktopHandler,
    handleClientEvents,
    lookupDataClient,

    -- * event handler registering
    withGuiEvents,
    withNamedGuiEvents,
    withDataEvents,

    -- * app
    addApp,

    -- * messages
    broadcastDraw,
    broadcastHtmlT,
    broadcastDesktopMessage,
    broadcastRawDesktopMessage,
) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Data.Text.Read qualified as Text

import Butler.App
import Butler.Clock
import Butler.Display
import Butler.Frame
import Butler.GUI
import Butler.Logger
import Butler.Memory
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
    , apps :: TVar (Map Pid AppInstance)
    , sysGuiHandlers :: TVar (Map TriggerName (Pipe GuiEvent))
    -- ^ system event handler, e.g. for window manager and tray
    , guiHandlers :: GuiHandlers
    -- ^ gui app handler, e.g. for htmx trigger
    , dataHandlers :: DataHandlers
    -- ^ data handler, e.g. for window manager and xterm
    , channels :: TVar (Map ChannelName (DisplayClient -> ProcessIO ()))
    -- ^ dedicated websocket, e.g. for novnc
    , clients :: DisplayClients
    -- ^ data clients
    , tabClients :: TVar (Map (SessionID, TabID) DisplayClient)
    -- ^ data clients indexed by tab, to lookup from htmx client
    , hclients :: DisplayClients
    -- ^ htmx clients
    , soundCard :: SoundCard
    }

newDesktop :: ProcessEnv -> Display -> Workspace -> WindowManager -> STM Desktop
newDesktop processEnv display ws wm = do
    Desktop processEnv display ws wm
        <$> newTVar mempty
        <*> newTVar mempty
        <*> newGuiHandlers
        <*> newDataHandlers
        <*> newTVar mempty
        <*> newDisplayClients
        <*> newTVar mempty
        <*> newDisplayClients
        <*> newSoundCard

withGuiEvents :: Desktop -> WithGuiEvents
withGuiEvents desktop cb wid pipe = do
    withGuiHandlers desktop.guiHandlers wid \events -> do
        cb (GuiEvents desktop.hclients events) wid pipe

withNamedGuiEvents :: Desktop -> WithNamedGuiEvents
withNamedGuiEvents desktop triggers cb wid pipe = do
    events <- atomically do
        gpipe <- newPipe
        let triggerMap = Map.fromList (map (\t -> (t, gpipe)) triggers)
        modifyTVar' desktop.sysGuiHandlers (Map.union triggerMap)
        pure gpipe
    cb (GuiEvents desktop.hclients events) wid pipe

withDataEvents :: Desktop -> WithDataEvents
withDataEvents desktop cb wid pipe = do
    withDataHandler desktop.dataHandlers \chan pipeData -> do
        cb (DataEvents desktop.clients chan pipeData) wid pipe

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
        , size = Nothing
        , start = pureHtmlApp draw
        }

startDesktop :: MVar Desktop -> (Desktop -> AppSet) -> (Desktop -> ProcessIO ()) -> Display -> Workspace -> ProcessIO ()
startDesktop desktopMVar mkAppSet xinit display name = do
    desktop <- newDesktopIO display name
    let appSet = mkAppSet desktop

    -- Reserve data chan for soundcard and winmanager
    atomically (reserveDataHandlers desktop.dataHandlers 2)

    let mkDeskApp draw = startApp (deskApp draw)

    xinit desktop

    apps <- atomically $ readMemoryVar desktop.wm.apps
    case Map.toList apps of
        [] -> do
            (wid, _) <- atomically $ newWindow desktop.wm.windows "Welcome"
            atomically . addWinApp desktop =<< mkDeskApp (welcomeWin appSet) wid
        xs -> forM_ xs $ \(wid, prog) -> do
            mApp <- case prog of
                "app-welcome" -> Just <$> mkDeskApp (welcomeWin appSet) wid
                "app-launcher" -> Just <$> mkDeskApp (menuWin appSet) wid
                _ -> launchApp appSet prog wid
            case mApp of
                Just app -> atomically $ addApp desktop app
                Nothing -> logError "Couldn't start app" ["wid" .= wid, "prog" .= prog]

    putMVar desktopMVar desktop

    -- TODO: watch running app and handle crash gracefully

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
addApp :: Desktop -> AppInstance -> STM ()
addApp desktop app = modifyTVar' desktop.apps (Map.insert app.process.pid app)

-- | Remove a registered gui app
delApp :: Desktop -> AppInstance -> STM ()
delApp desktop app = do
    modifyTVar' desktop.apps (Map.delete app.process.pid)
    void $ stopProcess desktop.env.os.processor app.process.pid

-- | Register a new windowed application.
addWinApp :: Desktop -> AppInstance -> STM ()
addWinApp desktop app = do
    addApp desktop app
    addWindowApp desktop.wm app.wid app.process

delWinApp :: Desktop -> WinID -> STM ()
delWinApp desktop wid = do
    prevApps <- stateTVar desktop.apps \apps ->
        let (winApps, rest) = Map.partition (\a -> a.wid == wid) apps
         in (Map.elems winApps, rest)
    traverse_ (delApp desktop) prevApps

swapGuiApp :: Desktop -> AppInstance -> STM ()
swapGuiApp desktop app = do
    delWinApp desktop app.wid
    addWinApp desktop app

broadcastRawDesktopMessage :: Desktop -> (DisplayClient -> Bool) -> ByteString -> ProcessIO ()
broadcastRawDesktopMessage desktop fpred bs = do
    -- logTrace "broadcast desktop" ["msg" .= BSLog msg]
    clients <- filter fpred <$> atomically (getClients desktop.clients)
    forM_ clients \client -> atomically $ sendBinary client (from bs)

broadcastDesktopMessage :: Desktop -> (DisplayClient -> Bool) -> ChannelID -> ByteString -> ProcessIO ()
broadcastDesktopMessage desktop fpred chan bs = do
    broadcastRawDesktopMessage desktop fpred (encodeMessage chan bs)

_writeHtml :: MonadIO m => TVar Text -> Html () -> m ()
_writeHtml t h = atomically $ writeTVar t (from $ renderText h)

desktopHtml :: Windows -> HtmlT STM ()
desktopHtml windows = do
    wids <- lift (getWindowIDs windows)
    div_ [id_ "display-root", class_ "flex flex-col min-h-full"] do
        script_ clientScript
        -- [style_ " grid-template-columns: repeat(auto-fill, minmax(600px, 1fr));", class_ "grid"]
        with div_ [id_ "win-root", class_ "flex grow min-h-full"] do
            mempty

        -- bottom bar
        with nav_ [id_ "display-menu", class_ "h-9 flex-none bg-slate-700 p-1 shadow w-full flex text-white shrink sticky bottom-0 z-50"] do
            with' div_ "grow" do
                with span_ [class_ "font-semibold mr-5", hxTrigger_ "click", id_ "wm-start", wsSend] ">>= start"
                with span_ [id_ "display-bar"] do
                    forM_ wids \wid -> with span_ [id_ (withWID wid "bar")] mempty
            with' div_ "display-bar-right" do
                with span_ [id_ "display-tray", class_ "flex h-full w-full align-center jusity-center"] do
                    forM_ wids \wid -> with span_ [id_ (withWID wid "tray")] mempty
                    with span_ [id_ "tray-0"] mempty
                    statusHtml False

        with div_ [id_ "reconnect_script"] mempty

        with div_ [id_ "backstore"] do
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

desktopHandler :: AppSet -> Desktop -> DisplayEvent -> ProcessIO ()
desktopHandler appSet desktop event = do
    -- update desktop state with display event
    case event of
        UserConnected chan client -> do
            spawnSendThread client

            atomically do
                when (chan == "data") do
                    addClient desktop.clients client
                    modifyTVar' desktop.tabClients (Map.insert (client.session.sessionID, client.tabID) client)
                when (chan == "htmx") do
                    addClient desktop.hclients client
                    -- Send the desktop body
                    sendHtml client (desktopHtml desktop.wm.windows)

            -- Notify each apps
            forwardDisplayEvent event

            handleNewUser chan client
        UserDisconnected chan client -> do
            atomically do
                when (chan == "data") do
                    delClient desktop.clients client
                    delSoundClient desktop.soundCard client
                    modifyTVar' desktop.tabClients (Map.delete (client.session.sessionID, client.tabID))
                when (chan == "htmx") do
                    delClient desktop.hclients client

            forwardDisplayEvent event
  where
    forwardDisplayEvent :: DisplayEvent -> ProcessIO ()
    forwardDisplayEvent devent = do
        apps <- readTVarIO desktop.apps
        forM_ apps \app -> writePipe app.pipeDisplayEvents devent

    handleNewUser :: ChannelName -> DisplayClient -> ProcessIO ()
    handleNewUser channel client = case channel of
        "htmx" -> do
            -- todo: play starting sound
            sleep 500

            atomically $ sendHtml client do
                with div_ [id_ "reconnect_script"] do
                    -- After connection, we switch the socket url to the reload endpoint,
                    -- so that after disconnection, it will receive new instruction, e.g. reload the page.
                    script_ "htmx.find('#display-ws').setAttribute('ws-connect', '/ws/htmx?reconnect=true')"

            -- wait for user events
            handleClientEvents client \mWinID trigger value -> case mWinID of
                Nothing ->
                    atomically (Map.lookup trigger <$> readTVar desktop.sysGuiHandlers) >>= \case
                        Nothing -> handleDesktopGuiEvent appSet desktop client trigger value
                        Just p -> writePipe p (GuiEvent client trigger value)
                Just wid -> do
                    atomically (lookupGuiHandler desktop.guiHandlers wid) >>= \case
                        Nothing -> logError "Unknown wid" ["v" .= value]
                        Just p -> writePipe p (GuiEvent client trigger value)
        "data" -> do
            spawnPingThread client

            forever do
                buf <- recvMessage client

                case decodeMessage buf of
                    Nothing -> logError "invalid data message" ["buf" .= BSLog buf]
                    Just (chan, bs)
                        | chan == audioChannel -> soundHandler desktop.soundCard client bs
                        | chan == winChannel -> tryHandleWinEvent desktop client bs
                        | otherwise -> do
                            atomically (lookupDataHandler desktop.dataHandlers chan) >>= \case
                                Nothing -> logError "unknown chan" ["chan" .= chan, "data" .= BSLog buf]
                                Just p -> writePipe p (DataEvent client bs buf)
        _ -> do
            channels <- readTVarIO desktop.channels
            case Map.lookup channel channels of
                Just cb -> cb client
                Nothing -> logError "unknown channel" ["channel" .= channel]

{- | Remove winID from trigger name

>>> decodeTriggerName "toggle"
(Nothing,"toggle")
>>> decodeTriggerName "toggle-1"
(Just 1,"toggle")
-}
decodeTriggerName :: Text -> (Maybe WinID, TriggerName)
decodeTriggerName txt = (mWinID, TriggerName txtTrigger)
  where
    (txtPrefix, txtSuffix) = Text.breakOnEnd "-" txt
    (mWinID, txtTrigger) = case Text.decimal txtSuffix of
        Right (wid, "") -> (Just (WinID wid), Text.dropEnd 1 txtPrefix)
        _ -> (Nothing, txt)

handleClientEvents :: DisplayClient -> (Maybe WinID -> TriggerName -> Value -> ProcessIO ()) -> ProcessIO ()
handleClientEvents client cb = do
    forever do
        buf <- recvMessage client
        case decode' (from buf) of
            Just v -> do
                case v ^? key "HEADERS" . key "HX-Trigger" . _String of
                    Just triggerTxt ->
                        let (mWinID, trigger) = decodeTriggerName triggerTxt
                         in cb mWinID trigger v
                    Nothing -> logError "event without trigger" ["event" .= v]
            Nothing -> logError "Received unknown websocket data" ["buf" .= BSLog buf]

handleDesktopGuiEvent :: AppSet -> Desktop -> DisplayClient -> TriggerName -> Value -> ProcessIO ()
handleDesktopGuiEvent appSet desktop _client trigger value = case trigger of
    "win-swap" -> do
        case (value ^? key "prog" . _String, value ^? key "win" . _Integer) of
            (Just (ProgramName -> appName), Just (WinID . unsafeFrom -> winId)) -> do
                mGuiApp <- asProcess desktop.env (launchApp appSet appName winId)
                case mGuiApp of
                    Just guiApp -> swapWindow winId guiApp
                    Nothing -> logInfo "unknown win-swap prog" ["v" .= value]
            _ -> logInfo "unknown win-swap" ["v" .= value]
    "wm-start" -> do
        createNewWindow
    _otherwise -> logError "Unknown ev" ["v" .= value]
  where
    createNewWindow :: ProcessIO ()
    createNewWindow = do
        (wid, script) <- atomically do
            (winId, win) <- newWindow desktop.wm.windows "REPL"
            let script = renderWindow (winId, win)
            pure (winId, script)

        guiApp <- asProcess desktop.env (startApp (deskApp $ menuWin appSet) wid)
        atomically $ addWinApp desktop guiApp

        sendsHtml desktop.hclients do
            with div_ [id_ "backstore", hxSwapOob_ "beforeend"] do
                with div_ [id_ (withWID wid "w")] do
                    welcomeWin appSet wid
                with (script_ script) [type_ "module"]
            with div_ [id_ "display-bar", hxSwapOob_ "afterbegin"] do
                with span_ [id_ (withWID wid "bar")] mempty
            with div_ [id_ "display-tray", hxSwapOob_ "afterbegin"] do
                with span_ [id_ (withWID wid "tray")] mempty

    swapWindow :: WinID -> AppInstance -> ProcessIO ()
    swapWindow winId guiApp = do
        clients <- atomically do
            swapGuiApp desktop guiApp
            getClients desktop.hclients
        forM_ clients \client -> writePipe guiApp.pipeDisplayEvents (UserConnected "htmx" client)
        broadcastTitle winId (processID guiApp.process)
        case guiApp.app.size of
            Just size -> do
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

tryHandleWinEvent :: Desktop -> DisplayClient -> ByteString -> ProcessIO ()
tryHandleWinEvent desktop client buf = case decode' (from buf) of
    Just obj -> case (obj ^? key "ev" . _String, obj ^? key "w" . _Integer) of
        (Just winEvent, Just (WinID . unsafeFrom -> winId)) -> do
            handleWinEvent desktop client buf winEvent winId obj
        _ -> logError "invalid win event" ["buf" .= BSLog buf]
    Nothing -> logError "unknown win event" ["buf" .= BSLog buf]

handleWinEvent :: Desktop -> DisplayClient -> ByteString -> Text -> WinID -> Value -> ProcessIO ()
handleWinEvent desktop client buf ev wid v = do
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
                atomically do
                    delWinApp desktop wid
                    delWindowApp desktop.wm wid
                sendsHtml desktop.hclients do
                    with span_ [wid_ wid "bar", hxSwapOob_ "delete"] mempty
                    with span_ [wid_ wid "tray", hxSwapOob_ "delete"] mempty
                logInfo "Delete win" ["wid" .= wid]
                pure True
            ("focus", Nothing, Nothing) -> do
                pure True
            _ -> do
                logError "invalid win-event" ["v" .= v]
                pure False
    when doBroadcast do
        broadcastDesktopMessage desktop (\o -> o.endpoint /= client.endpoint) winChannel buf
