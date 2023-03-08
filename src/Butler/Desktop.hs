{- | This module contains the logic to host multiple 'App'.

TODO: support custom window-manager and theme.
-}
module Butler.Desktop (
    Desktop (..),
    newDesktop,
    newDesktopIO,
    startDesktop,
    desktopHandler,

    -- * app
    addApp,
    addDesktopApp,
) where

import Data.Map.Strict qualified as Map
import Lucid
import Lucid.Htmx

import Butler
import Butler.App
import Butler.Core
import Butler.Core.Logger
import Butler.Core.Processor
import Butler.Display.WebSocket
import Butler.Frame

import Butler.Service.FileSystem

data Desktop = Desktop
    { env :: ProcessEnv
    , display :: Display
    , workspace :: Workspace
    , wm :: WindowManager
    , clients :: DisplayClients
    , shared :: AppSharedContext
    }

newDesktop :: ProcessEnv -> Display -> Workspace -> WindowManager -> STM Desktop
newDesktop processEnv display ws wm = do
    Desktop processEnv display ws wm
        <$> newDisplayClients
        <*> newAppSharedContext display

controlWin :: WinID
controlWin = WinID 0

newDesktopIO :: Display -> Workspace -> [Service] -> ProcessIO Desktop
newDesktopIO display ws services = do
    processEnv <- ask
    -- reserve wid for services
    let minWID = WinID (length services)
    wm <- newWindowManager minWID
    atomically (newDesktop processEnv display ws wm)

deskApp :: Desktop -> AppSet -> (WinID -> HtmlT STM ()) -> App
deskApp desktop appSet draw = defaultApp "welcome" startWelcomeApp
  where
    startWelcomeApp ctx = forever do
        atomically (readPipe ctx.pipe) >>= \case
            AppDisplay (UserConnected "htmx" client) -> atomically $ sendHtml client (draw ctx.wid)
            AppTrigger te -> case te.trigger of
                "win-swap" ->
                    case (te.body ^? key "prog" . _String, te.body ^? key "win" . _Integer) of
                        (Just (ProgramName -> appName), Just (WinID . unsafeFrom -> winId)) -> do
                            mGuiApp <- asProcess desktop.env (launchApp appSet appName desktop.shared ctx.clients winId)
                            case mGuiApp of
                                Just guiApp -> swapWindow winId guiApp
                                Nothing -> logInfo "unknown win-swap prog" ["v" .= te.body]
                        _ -> logInfo "unknown win-swap" ["v" .= te.body]
                _ -> logInfo "unknown ev" ["ev" .= te]
            _ -> pure ()

    swapWindow :: WinID -> AppInstance -> ProcessIO ()
    swapWindow wid guiApp = do
        clients <- atomically do
            swapApp desktop guiApp
            getClients desktop.clients
        forM_ clients \client -> writePipe guiApp.pipeAE (AppDisplay $ UserConnected "htmx" client)
        broadcastWinMessage ["w" .= wid, "ev" .= ("title" :: Text), "title" .= processID guiApp.process]
        case guiApp.app.size of
            Just size -> do
                broadcastSize wid size
                void $ atomically $ updateWindow desktop.wm.windows wid (#size .~ size)
            Nothing -> pure ()

    broadcastWinMessage body =
        sendsBinary desktop.clients (encodeMessageL controlWin (encodeJSON $ object body))

    broadcastSize :: WinID -> (Int, Int) -> ProcessIO ()
    broadcastSize wid (x, y) =
        broadcastWinMessage ["w" .= wid, "ev" .= ("resize" :: Text), "x" .= x, "y" .= y]

startDesktop :: MVar Desktop -> (Desktop -> AppSet) -> [Service] -> Display -> Workspace -> ProcessIO ()
startDesktop desktopMVar mkAppSet services display name = do
    desktop <- newDesktopIO display name services
    let appSet = mkAppSet desktop

    let mkDeskApp draw = startApp "app-" (deskApp desktop appSet draw) desktop.shared desktop.clients

    forM_ (zip [1 ..] services) \(wid, Service service) -> do
        atomically . addDesktopApp desktop =<< startApp "srv-" service desktop.shared desktop.clients (WinID wid)

    apps <- atomically $ readMemoryVar desktop.wm.apps
    case Map.toList apps of
        [] -> do
            (wid, _) <- atomically $ newWindow desktop.wm.windows "Welcome"
            atomically . addApp desktop =<< mkDeskApp (welcomeWin appSet) wid
        xs -> do
            logInfo "Restoring apps" ["apps" .= xs]
            forM_ xs $ \(wid, prog) -> do
                mApp <- case prog of
                    "app-welcome" -> Just <$> mkDeskApp (welcomeWin appSet) wid
                    "app-launcher" -> Just <$> mkDeskApp (menuWin appSet) wid
                    _ -> launchApp appSet prog desktop.shared desktop.clients wid
                case mApp of
                    Just app -> do
                        whenM (isNothing <$> atomically (lookupWindow desktop.wm.windows wid)) do
                            logError "Missing window for app" ["prog" .= prog]
                            atomically $ addWindowApp desktop.wm wid app.process
                        if coerce wid <= length services
                            then logError "Can't restore app, conflict with services" ["app" .= prog, "wid" .= wid]
                            else atomically $ addDesktopApp desktop app
                    Nothing -> logError "Couldn't start app" ["wid" .= wid, "prog" .= prog]

    putMVar desktopMVar desktop

    -- TODO: watch running app and handle crash gracefully

    -- This act as a ping thread
    let updateStatus s = do
            sendsHtml desktop.clients $ statusHtml s
            sleep 5_000
            updateStatus (not s)
    _ <- updateStatus True
    error "update status crashed?"

-- | Remove a registered gui app
delApp :: Desktop -> AppInstance -> STM ()
delApp desktop app = do
    unregisterApp desktop.shared.apps app
    void $ stopProcess app.process

-- | Register a new windowed application.
addApp :: Desktop -> AppInstance -> STM ()
addApp desktop app = do
    addDesktopApp desktop app
    addWindowApp desktop.wm app.wid app.process

-- | Add app without registering a window.
addDesktopApp :: Desktop -> AppInstance -> STM ()
addDesktopApp desktop app = do
    registerApp desktop.shared.apps app

swapApp :: Desktop -> AppInstance -> STM ()
swapApp desktop app = do
    Map.lookup app.wid <$> getApps desktop.shared.apps >>= \case
        Nothing -> pure ()
        Just prevApp -> delApp desktop prevApp
    addApp desktop app

delWin :: Desktop -> WinID -> STM ()
delWin desktop wid = do
    Map.lookup wid <$> getApps desktop.shared.apps >>= \case
        Nothing -> pure ()
        Just prevApp -> delApp desktop prevApp
    delWindowApp desktop.wm wid

desktopHtml :: [Service] -> Windows -> HtmlT STM ()
desktopHtml services windows = do
    wids <- lift (getWindowIDs windows)
    div_ [id_ "display-wins", class_ "flex flex-col min-h-full"] do
        script_ butlerHelpersScript
        -- [style_ " grid-template-columns: repeat(auto-fill, minmax(600px, 1fr));", class_ "grid"]
        with div_ [id_ "win-root", class_ "flex grow min-h-full"] do
            filesUploadButton (WinID 0)

        -- bottom bar
        with nav_ [id_ "display-menu", class_ "h-9 flex-none bg-slate-700 p-1 shadow w-full flex text-white shrink sticky bottom-0 z-50"] do
            with' div_ "grow" do
                with span_ [class_ "font-semibold mr-5", hxTrigger_ "click", wid_ (WinID 0) "wm-start", wsSend] ">>= start"
                with span_ [id_ "display-bar"] do
                    forM_ wids \wid -> with span_ [wid_ wid "bar"] mempty
            with' div_ "display-bar-right" do
                with span_ [id_ "display-tray", class_ "flex h-full w-full align-center jusity-center"] do
                    forM_ wids \wid -> with span_ [wid_ wid "tray"] mempty
                    with span_ [wid_ (WinID 0) "tray"] mempty
                    forM_ (zip [1 ..] services) \(WinID -> wid, _) ->
                        with span_ [wid_ wid "tray"] mempty

        with div_ [id_ "reconnect_script"] mempty

        with div_ [id_ "backstore"] do
            renderWindows controlWin windows

welcomeWin :: Monad m => AppSet -> WinID -> HtmlT m ()
welcomeWin appSet wid = do
    with div_ [wid_ wid "w", class_ "grid grid-cols-1 divide-y"] do
        with div_ [class_ "p-2"] do
            "Welcome to "
            with span_ [class_ "font-bold"] "ButlerOS"
        div_ do
            "Press start!"
        with div_ [class_ "m-2"] do
            appSetHtml wid appSet

menuWin :: Monad m => AppSet -> WinID -> HtmlT m ()
menuWin appSet wid = do
    with div_ [wid_ wid "w"] do
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

desktopHandler :: AppSet -> [Service] -> Desktop -> DisplayEvent -> ProcessIO ()
desktopHandler appSet services desktop event = do
    -- update desktop state with display event
    case event of
        UserConnected chan client -> do
            spawnThread_ (sendThread client)

            atomically do
                when (chan == "htmx") do
                    addClient desktop.clients client
                    -- Send the desktop body
                    sendHtml client (desktopHtml services desktop.wm.windows)

            -- Notify each apps
            forwardDisplayEvent event

            if chan == "htmx"
                then handleNewUser chan client
                else void $ pingThread client
        UserDisconnected chan client -> do
            atomically do
                when (chan == "htmx") do
                    delClient desktop.clients client

            forwardDisplayEvent event
  where
    forwardDisplayEvent :: DisplayEvent -> ProcessIO ()
    forwardDisplayEvent devent = do
        apps <- atomically (getApps desktop.shared.apps)
        forM_ apps \app -> writePipe app.pipeAE (AppDisplay devent)

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
            forever do
                dataMessage <- recvData client
                case eventFromMessage client dataMessage of
                    Nothing -> logError "Unknown data" ["ev" .= LBSLog (into @LByteString dataMessage)]
                    Just (wid, ae)
                        | wid == controlWin -> handleDesktopEvent appSet desktop ae
                        | otherwise ->
                            (Map.lookup wid <$> atomically (getApps desktop.shared.apps)) >>= \case
                                Nothing -> logError "Unknown wid" ["wid" .= wid, "ev" .= ae]
                                Just appInstance -> writePipe appInstance.pipeAE ae
        _ -> logError "unknown channel" ["channel" .= channel]

handleDesktopEvent :: AppSet -> Desktop -> AppEvent -> ProcessIO ()
handleDesktopEvent appSet desktop = \case
    AppData de -> case decode' (from de.buffer) of
        Just obj -> case (obj ^? key "ev" . _String, obj ^? key "w" . _Integer) of
            (Just winEvent, Just (WinID . unsafeFrom -> winId)) -> do
                handleWinEvent desktop de.client (from de.buffer) winEvent winId obj
            _ -> logError "invalid win event" ["buf" .= BSLog de.buffer]
        Nothing -> logError "unknown win event" ["buf" .= BSLog de.buffer]
    AppTrigger te -> handleDesktopGuiEvent appSet desktop te.client te.trigger te.body
    _ -> pure ()

handleDesktopGuiEvent :: AppSet -> Desktop -> DisplayClient -> TriggerName -> Value -> ProcessIO ()
handleDesktopGuiEvent appSet desktop _client trigger value = case trigger of
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

        guiApp <- asProcess desktop.env (startApp "app-" (deskApp desktop appSet $ menuWin appSet) desktop.shared desktop.clients wid)
        atomically $ addApp desktop guiApp

        sendsHtml desktop.clients do
            with div_ [id_ "backstore", hxSwapOob_ "beforeend"] do
                with div_ [id_ (withWID wid "w")] do
                    menuWin appSet wid
                with (script_ script) [type_ "module"]
            with div_ [id_ "display-bar", hxSwapOob_ "afterbegin"] do
                with span_ [id_ (withWID wid "bar")] mempty
            with div_ [id_ "display-tray", hxSwapOob_ "afterbegin"] do
                with span_ [id_ (withWID wid "tray")] mempty

handleWinEvent :: Desktop -> DisplayClient -> LByteString -> Text -> WinID -> Value -> ProcessIO ()
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
                    delWin desktop wid
                sendsHtml desktop.clients do
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
        sendsBinaryButSelf client desktop.clients (encodeMessageL controlWin buf)
