{- | The desktop app

TODO: support custom window-manager and theme.
-}
module Butler.App.Desktop (
    desktopApp,
    Desktop (..),
) where

import Data.IntMap.Strict qualified as IM
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Lucid
import Lucid.Htmx

import Butler
import Butler.App
import Butler.Core
import Butler.Core.Logger
import Butler.Core.Processor
import Butler.Display.Session
import Butler.Display.User
import Butler.Frame
import Butler.GUI.File

import Butler.Service.FileService

desktopApp :: [Service] -> App
desktopApp services = defaultApp "desktop" (startDesktopApp services)

controlWin :: WinID
controlWin = WinID 0

deskApp :: AppSet -> App
deskApp appSet = defaultApp "welcome" startWelcomeApp
  where
    startWelcomeApp ctx = forever do
        atomically (readPipe ctx.pipe) >>= \case
            ae@AppDisplay{} -> sendHtmlOnConnect (welcomeWin appSet ctx.wid) ae
            _ -> pure ()

startDesktopApp :: [Service] -> AppContext -> ProcessIO ()
startDesktopApp services ctx = do
    wm <- newWindowManager (WinID (length services))

    let startDeskApp = startApp "app-" (deskApp ctx.shared.appSet) ctx.shared

    forM_ (zip [1 ..] services) \(wid, Service service) -> do
        atomically . registerApp ctx.shared.apps =<< startApp "srv-" service ctx.shared (WinID wid)

    dir <- getVolumeDirectory ctx.shared (Just "Desktop")

    spawnThread_ $ renderOnChange (renderFileIcons controlWin dir) \newHtml -> do
        logInfo "Updating desktop directory ui" []
        sendsHtml ctx.shared.clients newHtml

    Map.toList <$> atomically (readMemoryVar wm.apps) >>= \case
        [] -> do
            (wid, _) <- atomically $ newWindow wm.windows "Welcome"
            atomically . addApp wm ctx.shared =<< startDeskApp wid
        xs -> do
            logInfo "Restoring apps" ["apps" .= xs]
            forM_ xs $ \(wid, prog) -> do
                mApp <- case prog of
                    "app-welcome" -> Just <$> startDeskApp wid
                    "app-launcher" -> launchApp ctx.shared.appSet "launcher" ctx.shared wid
                    _ -> launchApp ctx.shared.appSet prog ctx.shared wid
                case mApp of
                    Just app -> do
                        whenM (isNothing <$> atomically (lookupWindow wm.windows wid)) do
                            logError "Missing window for app" ["prog" .= prog]
                            atomically $ addWindowApp wm wid app.process
                        if coerce wid <= length services
                            then logError "Can't restore app, conflict with services" ["app" .= prog, "wid" .= wid]
                            else atomically $ registerApp ctx.shared.apps app
                    Nothing -> logError "Couldn't start app" ["wid" .= wid, "prog" .= prog]

    -- This act as a ping thread
    let updateStatus s = do
            sendsHtml ctx.shared.clients $ statusHtml s
            sleep 5_000
            updateStatus (not s)
    spawnThread_ (updateStatus True)

    let
        handleNewApp value = case value ^? key "name" . _JSON of
            Just name -> do
                let rootDir = getRootDir dir
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
                                        File file -> AppFile dir (Just file)
                                 in fmap toLookupResult <$> lookupChild dir fp
                case value ^? key "wid" . _JSON of
                    Just wid -> handleWinSwap wm ctx wid name mEvent
                    Nothing -> createNewWindow name mEvent
            Nothing -> logError "missing name" ["ev" .= value]
        createNewWindow :: ProgramName -> Maybe AppEvent -> ProcessIO ()
        createNewWindow name mEvent = do
            (wid, script) <- atomically do
                (winId, win) <- newWindow wm.windows (from name)
                let script = renderWindow (winId, win)
                pure (winId, script)

            mGuiApp <- launchApp ctx.shared.appSet name ctx.shared wid
            forM_ mGuiApp \guiApp -> do
                atomically $ addApp wm ctx.shared guiApp
                renderNewWindow wid script
                forM_ mEvent $ writePipe guiApp.pipe
                clients <- atomically (getClients ctx.shared.clients)
                forM_ clients \client -> writePipe guiApp.pipe (AppDisplay $ UserJoined client)

        renderNewWindow wid script = do
            sendsHtml ctx.shared.clients do
                with div_ [id_ "backstore", hxSwapOob_ "beforeend"] do
                    with div_ [id_ (withWID wid "w")] mempty
                    with (script_ script) [type_ "module"]
                with div_ [id_ "display-bar", hxSwapOob_ "afterbegin"] do
                    with span_ [id_ (withWID wid "bar")] mempty
                with div_ [id_ "display-tray", hxSwapOob_ "afterbegin"] do
                    with span_ [id_ (withWID wid "tray")] mempty

        handleWinEvent :: DisplayClient -> LByteString -> Text -> WinID -> Value -> ProcessIO ()
        handleWinEvent client buf ev wid v = do
            doBroadcast <-
                case ( ev
                     , unsafeFrom <$> (v ^? key "x" . _Integer)
                     , unsafeFrom <$> (v ^? key "y" . _Integer)
                     ) of
                    ("move", Just x, Just y) -> do
                        atomically $ updateWindow wm.windows wid (#position .~ (x, y))
                    ("resize", Just x, Just y) -> do
                        atomically $ updateWindow wm.windows wid (#size .~ (x, y))
                    ("close", Nothing, Nothing) -> do
                        atomically do
                            delWin wm ctx wid
                        sendsHtml (ctx.shared.clients) do
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
                sendsBinaryButSelf client ctx.shared.clients (encodeMessageL controlWin buf)

        desktop =
            Desktop
                { mountUI = desktopHtml services dir wm.windows
                , thumbnail = do
                    let attr :: Text -> HtmlT STM () -> HtmlT STM ()
                        attr k v =
                            tr_ do
                                with td_ [class_ "text-right pr-1"] do
                                    toHtml k
                                    ":"
                                with td_ [class_ "font-medium"] do
                                    v
                    clients <- lift (getClients ctx.shared.clients)
                    attr "clients" do
                        with div_ [class_ "flex"] do
                            traverse_ (\c -> userIcon =<< lift (readTVar c.session.username)) clients
                    wins <- IM.size . (.windows) <$> lift (readMemoryVar wm.windows)
                    attr "wins" (toHtml (show wins))
                }

    forever $ do
        atomically (readPipe ctx.pipe) >>= \case
            AppDisplay (UserJoined client) -> do
                -- todo: play starting sound
                sleep 500

                atomically $ sendHtml client do
                    with div_ [id_ "reconnect_script"] do
                        -- After connection, we switch the socket url to the reload endpoint,
                        -- so that after disconnection, it will receive new instruction, e.g. reload the page.
                        script_ "htmx.find('#display-ws').setAttribute('ws-connect', '/ws/htmx?reconnect=true')"
            AppDisplay _ -> pure ()
            AppData de -> case decode' (from de.buffer) of
                Just obj -> case (obj ^? key "ev" . _String, obj ^? key "w" . _Integer) of
                    (Just winEvent, Just (WinID . unsafeFrom -> winId)) -> do
                        handleWinEvent de.client (from de.buffer) winEvent winId obj
                    _ -> logError "invalid win event" ["buf" .= BSLog de.buffer]
                Nothing -> logError "unknown win event" ["buf" .= BSLog de.buffer]
            AppTrigger ev -> case ev.trigger of
                "wm-start" -> createNewWindow "launcher" Nothing
                "start-app" -> handleNewApp ev.body
                _otherwise -> logError "Unknown ev" ["ev" .= ev]
            AppSync ev ->
                atomically . putTMVar ev.reply . toDyn $ desktop
            ev -> logError "Unexpected event" ["ev" .= ev]

data Desktop = Desktop
    { mountUI :: HtmlT STM ()
    , thumbnail :: HtmlT STM ()
    }

-- | Remove a registered gui app
delApp :: AppSharedContext -> AppInstance -> STM ()
delApp shared app = do
    unregisterApp shared.apps app
    void $ stopProcess app.process

-- | Register a new windowed application.
addApp :: WindowManager -> AppSharedContext -> AppInstance -> STM ()
addApp wm shared app = do
    registerApp shared.apps app
    addWindowApp wm app.wid app.process

swapApp :: WindowManager -> AppSharedContext -> AppInstance -> STM ()
swapApp wm shared app = do
    Map.lookup app.wid <$> getApps shared.apps >>= \case
        Nothing -> pure ()
        Just prevApp -> delApp shared prevApp
    addApp wm shared app

delWin :: WindowManager -> AppContext -> WinID -> STM ()
delWin wm ctx wid = do
    Map.lookup wid <$> getApps ctx.shared.apps >>= \case
        Nothing -> pure ()
        Just prevApp -> delApp ctx.shared prevApp
    delWindowApp wm wid

desktopHtml :: [Service] -> Directory -> Windows -> HtmlT STM ()
desktopHtml services dir windows = do
    wids <- lift (getWindowIDs windows)
    div_ [id_ "display-wins", class_ "flex flex-col min-h-full"] do
        script_ butlerHelpersScript
        -- [style_ " grid-template-columns: repeat(auto-fill, minmax(600px, 1fr));", class_ "grid"]
        with div_ [id_ "win-root", class_ "flex grow"] do
            with div_ [class_ "flex flex-col"] do
                let deskDiv = "border border-black rounded mx-2 my-3 w-6 grid align-center justify-center cursor-pointer"
                withTrigger "click" controlWin "start-app" ["name" .= ProgramName "file-manager"] div_ [class_ deskDiv] do
                    with i_ [class_ "ri-computer-line"] mempty
                renderFileIcons controlWin dir
                filesUploadButton (WinID 0) (getFileLoc dir Nothing)

        -- bottom bar
        with nav_ [id_ "display-menu", class_ "h-9 flex-none bg-slate-700 p-1 shadow w-full flex text-white z-50"] do
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
                    statusHtml True

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
            , class_ $ "m-auto ml-1 h-3 w-3 center rounded-full opacity-75 cursor-pointer " <> cls
            , hxTrigger_ "click"
            , wsSend
            ]
            mempty

handleWinSwap :: WindowManager -> AppContext -> WinID -> ProgramName -> Maybe AppEvent -> ProcessIO ()
handleWinSwap wm ctx wid appName mEvent = do
    mGuiApp <- launchApp ctx.shared.appSet appName ctx.shared wid
    case mGuiApp of
        Just guiApp -> swapWindow guiApp
        Nothing -> logInfo "unknown win-swap prog" ["v" .= appName]
  where
    swapWindow :: AppInstance -> ProcessIO ()
    swapWindow guiApp = do
        atomically $ swapApp wm ctx.shared guiApp
        forM_ mEvent $ writePipe guiApp.pipe
        clients <- atomically $ getClients ctx.shared.clients
        forM_ clients \client -> writePipe guiApp.pipe (AppDisplay $ UserJoined client)
        broadcastWinMessage ["w" .= wid, "ev" .= ("title" :: Text), "title" .= processID guiApp.process]
        case guiApp.app.size of
            Just size -> do
                broadcastSize size
                void $ atomically $ updateWindow wm.windows wid (#size .~ size)
            Nothing -> pure ()

    broadcastWinMessage body =
        sendsBinary ctx.shared.clients (encodeMessageL controlWin (encodeJSON $ object body))

    broadcastSize :: (Int, Int) -> ProcessIO ()
    broadcastSize (x, y) =
        broadcastWinMessage ["w" .= wid, "ev" .= ("resize" :: Text), "x" .= x, "y" .= y]
