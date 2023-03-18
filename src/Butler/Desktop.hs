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
import Data.Text qualified as Text
import Lucid
import Lucid.Htmx

import Butler
import Butler.App
import Butler.Core
import Butler.Core.Logger
import Butler.Core.Processor
import Butler.Display.WebSocket
import Butler.Frame
import Butler.GUI.File

import Butler.Service.FileService

data Desktop = Desktop
    { env :: ProcessEnv
    , display :: Display
    , workspace :: Workspace
    , wm :: WindowManager
    , clients :: DisplayClients
    , shared :: AppSharedContext
    , directory :: TVar (Maybe Directory)
    }

newDesktop :: ProcessEnv -> Display -> Workspace -> WindowManager -> AppSet -> STM Desktop
newDesktop processEnv display ws wm appSet = do
    Desktop processEnv display ws wm
        <$> newDisplayClients
        <*> newAppSharedContext display processEnv appSet
        <*> newTVar Nothing

controlWin :: WinID
controlWin = WinID 0

newDesktopIO :: Display -> Workspace -> [Service] -> AppSet -> ProcessIO Desktop
newDesktopIO display ws services appSet = do
    processEnv <- ask
    -- reserve wid for services
    let minWID = WinID (length services)
    wm <- newWindowManager minWID
    atomically (newDesktop processEnv display ws wm appSet)

deskApp :: AppSet -> App
deskApp appSet = defaultApp "welcome" startWelcomeApp
  where
    startWelcomeApp ctx = forever do
        atomically (readPipe ctx.pipe) >>= \case
            ae@AppDisplay{} -> sendHtmlOnConnect (welcomeWin appSet ctx.wid) ae
            _ -> pure ()

startDesktop :: MVar Desktop -> AppSet -> [Service] -> Display -> Workspace -> ProcessIO ()
startDesktop desktopMVar appSet services display name = do
    desktop <- newDesktopIO display name services appSet

    let startDeskApp = startApp "app-" (deskApp appSet) desktop.shared desktop.clients

    forM_ (zip [1 ..] services) \(wid, Service service) -> do
        atomically . addDesktopApp desktop =<< startApp "srv-" service desktop.shared desktop.clients (WinID wid)

    dir <- getVolumeDirectory desktop.shared (Just "Desktop")
    atomically do
        writeTVar desktop.directory (Just dir)

    spawnThread_ $ renderOnChange (renderFileIcons controlWin dir) \newHtml -> do
        logInfo "Updating desktop directory ui" []
        sendsHtml desktop.clients newHtml

    apps <- atomically $ readMemoryVar desktop.wm.apps
    case Map.toList apps of
        [] -> do
            (wid, _) <- atomically $ newWindow desktop.wm.windows "Welcome"
            atomically . addApp desktop =<< startDeskApp wid
        xs -> do
            logInfo "Restoring apps" ["apps" .= xs]
            forM_ xs $ \(wid, prog) -> do
                mApp <- case prog of
                    "app-welcome" -> Just <$> startDeskApp wid
                    "app-launcher" -> launchApp appSet "launcher" desktop.shared desktop.clients wid
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

    -- This act as a ping thread
    let updateStatus s = do
            sendsHtml desktop.clients $ statusHtml s
            sleep 5_000
            updateStatus (not s)

    -- TODO: watch running app and handle crash gracefully
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

desktopHtml :: Desktop -> [Service] -> Windows -> HtmlT STM ()
desktopHtml desktop services windows = do
    wids <- lift (getWindowIDs windows)
    div_ [id_ "display-wins", class_ "flex flex-col min-h-full"] do
        script_ butlerHelpersScript
        -- [style_ " grid-template-columns: repeat(auto-fill, minmax(600px, 1fr));", class_ "grid"]
        with div_ [id_ "win-root", class_ "flex grow min-h-full"] do
            with div_ [class_ "flex flex-col"] do
                let deskDiv = "border border-black rounded mx-2 my-3 w-6 grid align-center justify-center cursor-pointer"
                withTrigger "click" controlWin "start-app" ["name" .= ProgramName "file-manager"] div_ [class_ deskDiv] do
                    with i_ [class_ "ri-computer-line"] mempty
                lift (readTVar desktop.directory) >>= \case
                    Nothing -> pure ()
                    Just dir -> do
                        renderFileIcons controlWin dir
                        filesUploadButton (WinID 0) (getFileLoc dir Nothing)

        -- bottom bar
        with nav_ [id_ "display-menu", class_ "h-9 flex-none bg-slate-700 p-1 shadow w-full flex text-white shrink sticky bottom-0 z-50"] do
            with' div_ "grow" do
                with span_ [class_ "font-semibold mr-5", hxTrigger_ "click", wid_ (WinID 0) "wm-start", wsSend] ">>= start"
                with span_ [id_ "display-bar"] do
                    forM_ wids \wid -> with span_ [wid_ wid "bar"] mempty
            with' div_ "display-bar-right" do
                with span_ [id_ "display-tray", class_ "flex h-full w-full align-center justify-center"] do
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
                    sendHtml client (desktopHtml desktop services desktop.wm.windows)

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
        forM_ apps \app -> writePipe app.pipe (AppDisplay devent)

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
                                Just appInstance -> writePipe appInstance.pipe ae
        _ -> logError "unknown channel" ["channel" .= channel]

handleWinSwap :: AppSet -> Desktop -> WinID -> ProgramName -> Maybe AppEvent -> ProcessIO ()
handleWinSwap appSet desktop wid appName mEvent = do
    mGuiApp <- asProcess desktop.env (launchApp appSet appName desktop.shared desktop.clients wid)
    case mGuiApp of
        Just guiApp -> swapWindow guiApp
        Nothing -> logInfo "unknown win-swap prog" ["v" .= appName]
  where
    swapWindow :: AppInstance -> ProcessIO ()
    swapWindow guiApp = do
        clients <- atomically do
            swapApp desktop guiApp
            getClients desktop.clients
        forM_ mEvent $ writePipe guiApp.pipe
        forM_ clients \client -> writePipe guiApp.pipe (AppDisplay $ UserConnected "htmx" client)
        broadcastWinMessage ["w" .= wid, "ev" .= ("title" :: Text), "title" .= processID guiApp.process]
        case guiApp.app.size of
            Just size -> do
                broadcastSize size
                void $ atomically $ updateWindow desktop.wm.windows wid (#size .~ size)
            Nothing -> pure ()

    broadcastWinMessage body =
        sendsBinary desktop.clients (encodeMessageL controlWin (encodeJSON $ object body))

    broadcastSize :: (Int, Int) -> ProcessIO ()
    broadcastSize (x, y) =
        broadcastWinMessage ["w" .= wid, "ev" .= ("resize" :: Text), "x" .= x, "y" .= y]

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
    "wm-start" -> createNewWindow "launcher" Nothing
    "start-app" -> handleNewApp
    _otherwise -> logError "Unknown ev" ["v" .= value]
  where
    handleNewApp = case value ^? key "name" . _JSON of
        Just name -> do
            desktopDir <- fromMaybe (error "missing root dir") <$> readTVarIO desktop.directory
            let rootDir = getRootDir desktopDir
            mEvent <- atomically do
                case value ^? key "fp" . _JSON of
                    Nothing -> pure $ Just (AppFile rootDir Nothing)
                    Just fp
                        | -- FP is a global FileLoc
                          '/' `Text.elem` from fp ->
                            fmap (uncurry AppFile) <$> resolveFileLoc rootDir (from fp)
                        | otherwise ->
                            let toLookupResult = \case
                                    Directory d -> AppFile d Nothing
                                    File file -> AppFile desktopDir (Just file)
                             in fmap toLookupResult <$> lookupChild desktopDir fp
            case value ^? key "wid" . _JSON of
                Just wid -> handleWinSwap appSet desktop wid name mEvent
                Nothing -> createNewWindow name mEvent
        Nothing -> logError "missing name" ["ev" .= value]
    createNewWindow :: ProgramName -> Maybe AppEvent -> ProcessIO ()
    createNewWindow name mEvent = do
        (wid, script) <- atomically do
            (winId, win) <- newWindow desktop.wm.windows (from name)
            let script = renderWindow (winId, win)
            pure (winId, script)

        mGuiApp <- asProcess desktop.env (launchApp appSet name desktop.shared desktop.clients wid)
        forM_ mGuiApp \guiApp -> do
            atomically $ addApp desktop guiApp
            renderNewWindow wid script
            clients <- atomically $ getClients desktop.clients
            forM_ mEvent $ writePipe guiApp.pipe
            forM_ clients \client -> writePipe guiApp.pipe (AppDisplay $ UserConnected "htmx" client)

    renderNewWindow wid script = do
        sendsHtml desktop.clients do
            with div_ [id_ "backstore", hxSwapOob_ "beforeend"] do
                with div_ [id_ (withWID wid "w")] mempty
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
