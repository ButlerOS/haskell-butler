{- | The desktop app

TODO: support custom window-manager and theme.
-}
module Butler.App.Desktop (
    desktopApp,
    Desktop (..),
) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Lucid
import Lucid.Htmx

import Butler
import Butler.App
import Butler.AppID
import Butler.AppSettings
import Butler.Core
import Butler.Core.Logger
import Butler.Core.Processor
import Butler.Display.Session
import Butler.Display.User
import Butler.Frame
import Butler.GUI.File

import Butler.Service.FileService

desktopApp :: [Service] -> App
desktopApp services =
    (defaultApp "desktop" (startDesktopApp services))
        { settings =
            [ AppSetting "background" "bg-stone-100" (SettingChoice ["bg-stone-100", "bg-blue-100"])
            ]
        }

deskApp :: AppSet -> App
deskApp appSet = defaultApp "welcome" startWelcomeApp
  where
    startWelcomeApp ctx = forever do
        atomically (readPipe ctx.pipe) >>= \case
            ae@AppDisplay{} -> sendHtmlOnConnect (welcomeWin appSet ctx.wid) ae
            _ -> pure ()

startDesktopApp :: [Service] -> AppContext -> ProcessIO ()
startDesktopApp services ctx = do
    wm <- newWindowManager

    appSettings <- getAppSettings ctx.shared "desktop"
    vBgColor <-
        newTVarIO =<< case Map.lookup "background" appSettings of
            Nothing -> do
                logError "Missing background color" ["setting" .= appSettings]
                pure ""
            Just v -> pure v
    logDebug "current setting" ["setting" .= appSettings]

    let startDeskApp = startApp "app-" (deskApp ctx.shared.appSet) Nothing ctx.shared

    forM_ services \(Service service) -> do
        wid <- atomically (nextAppID ctx.shared.appIDCounter =<< getApps ctx.shared.apps)
        atomically . registerApp ctx.shared.apps =<< startApp "srv-" service Nothing ctx.shared wid

    dir <- getVolumeDirectory ctx.shared (Just "Desktop")

    spawnThread_ $ renderOnChange (renderFileIcons shellAppID dir) \newHtml -> do
        logInfo "Updating desktop directory ui" []
        sendsHtml ctx.shared.clients newHtml

    Map.toList <$> atomically (readMemoryVar wm.apps) >>= \case
        [] -> do
            wid <- atomically (nextAppID ctx.shared.appIDCounter =<< getApps ctx.shared.apps)
            void $ atomically $ newWindow wm.windows wid "Welcome"
            delAppMemory wid
            atomically . addApp wm ctx.shared =<< startDeskApp wid
        xs -> do
            logInfo "Restoring apps" ["apps" .= xs]
            forM_ xs $ \(wid, (argv, prog)) -> do
                mApp <- case prog of
                    "app-welcome" -> Just <$> startDeskApp wid
                    "app-launcher" -> launchApp ctx.shared.appSet "launcher" Nothing ctx.shared wid
                    _ -> launchApp ctx.shared.appSet prog argv ctx.shared wid
                case mApp of
                    Just app -> do
                        whenM (isNothing <$> atomically (lookupWindow wm.windows wid)) do
                            logError "Missing window for app" ["prog" .= prog]
                            atomically $ addWindowApp wm app
                        if from wid <= length services
                            then logError "Can't restore app, conflict with services" ["app" .= prog, "wid" .= wid]
                            else atomically $ registerApp ctx.shared.apps app
                    Nothing -> logError "Couldn't start app" ["wid" .= wid, "prog" .= prog]

    let handleNewApp value = case value ^? key "name" . _JSON of
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
                let argv = value ^? key "argv" . _JSON
                case value ^? key "wid" . _JSON of
                    Just wid -> handleWinSwap wm ctx wid name argv mEvent
                    Nothing -> createNewWindow name argv mEvent
            Nothing -> logError "missing name" ["ev" .= value]

        createNewWindow :: ProgramName -> Maybe Value -> Maybe AppEvent -> ProcessIO ()
        createNewWindow name argv mEvent = do
            (wid, win) <- atomically do
                wid <- nextAppID ctx.shared.appIDCounter =<< getApps ctx.shared.apps
                win <- newWindow wm.windows wid (from name)
                pure (wid, win)

            renderNewWindow wid win

            delAppMemory wid
            mGuiApp <- launchApp ctx.shared.appSet name argv ctx.shared wid
            forM_ mGuiApp \guiApp -> do
                atomically $ addApp wm ctx.shared guiApp
                forM_ mEvent $ writePipe guiApp.pipe
                clients <- atomically (getClients ctx.shared.clients)
                forM_ clients \client -> writePipe guiApp.pipe (AppDisplay $ UserJoined client)

        renderNewWindow wid win = do
            sendsHtml ctx.shared.clients do
                with div_ [id_ "backstore", hxSwapOob_ "beforeend"] do
                    with div_ [wid_ wid "w"] mempty
                    with (script_ $ renderWindow (wid, win)) [type_ "module"]
                with div_ [id_ "display-bar", hxSwapOob_ "afterbegin"] do
                    with span_ [wid_ wid "bar"] mempty
                with div_ [id_ "display-tray", hxSwapOob_ "afterbegin"] do
                    with span_ [wid_ wid "tray"] mempty

        handleWinEvent :: DisplayClient -> DataEvent -> Text -> AppID -> Value -> ProcessIO ()
        handleWinEvent client ev name wid obj = do
            doBroadcast <-
                case ( name
                     , unsafeFrom <$> (obj ^? key "x" . _Integer)
                     , unsafeFrom <$> (obj ^? key "y" . _Integer)
                     ) of
                    ("move", Just x, Just y) -> do
                        atomically $ updateWindow wm.windows wid (#position .~ (x, y))
                    ("resize", Just x, Just y) -> do
                        atomically $ updateWindow wm.windows wid (#size .~ (x, y))
                    ("close", Nothing, Nothing) -> do
                        atomically do
                            delWin wm ctx wid
                            -- Close event are not performed client side and we to trigger the handler manually
                            sendBinary client (from ev.rawBuffer)
                        delAppMemory wid
                        sendsHtml (ctx.shared.clients) do
                            with span_ [wid_ wid "bar", hxSwapOob_ "delete"] mempty
                            with span_ [wid_ wid "tray", hxSwapOob_ "delete"] mempty
                        logInfo "Delete win" ["wid" .= wid]
                        pure True
                    ("focus", Nothing, Nothing) -> do
                        pure True
                    _ -> do
                        logError "invalid win-event" ["ev" .= ev]
                        pure False
            when doBroadcast do
                sendsBinaryButSelf client ctx.shared.clients (from ev.rawBuffer)

    acls <- newTVarIO mempty
    let desktop =
            Desktop
                { mountUI = desktopHtml vBgColor ctx.shared.apps dir wm.windows
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
                    wins <- Map.size . (.windows) <$> lift (readMemoryVar wm.windows)
                    attr "wins" (toHtml (show wins))
                , acls
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
                Just obj -> case (obj ^? key "ev" . _String, obj ^? key "w" . _JSON) of
                    (Just winEvent, Just winId) -> do
                        handleWinEvent de.client de winEvent winId obj
                    _ -> logError "invalid win event" ["buf" .= LBSLog de.buffer]
                Nothing -> logError "unknown win event" ["buf" .= LBSLog de.buffer]
            AppTrigger ev -> case ev.trigger of
                "wm-start" -> createNewWindow "launcher" Nothing Nothing
                "start-app" -> handleNewApp ev.body
                _otherwise -> logError "Unknown ev" ["ev" .= ev]
            AppSync ev -> case ev.name of
                "desktop-ui" -> atomically . putTMVar ev.reply . toDyn $ desktop
                _ -> logError "Unknown en" ["ev" .= ev]
            AppSettingChanged setting prev new -> case setting of
                "background" -> do
                    sendsHtml ctx.shared.clients do
                        with div_ [id_ "background_script"] do
                            script_ $ "changeBackground" <> showT (prev, new)
                    atomically $ writeTVar vBgColor new
                _ -> logError "Unknown setting" ["setting" .= setting]
            ev -> logError "Unexpected event" ["ev" .= ev]

desktopScript :: Text
desktopScript =
    [raw|
globalThis.changeBackground = (prev, next) => {
  const elt = htmx.find("#win-root")
  htmx.removeClass(elt, prev)
  htmx.addClass(elt, next)
}
|]

data Desktop = Desktop
    { mountUI :: HtmlT STM ()
    , thumbnail :: HtmlT STM ()
    , acls :: TVar [Session]
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
    addWindowApp wm app

delWin :: WindowManager -> AppContext -> AppID -> STM ()
delWin wm ctx wid = do
    Map.lookup wid <$> getApps ctx.shared.apps >>= \case
        Nothing -> pure ()
        Just prevApp -> delApp ctx.shared prevApp
    delWindowApp wm wid

desktopHtml :: TVar SettingValue -> Apps -> Directory -> Windows -> HtmlT STM ()
desktopHtml vBgColor apps dir windows = do
    wids <- lift (getWindowIDs windows)
    div_ [id_ "display-wins", class_ "flex flex-col min-h-full"] do
        script_ (butlerHelpersScript <> "\n" <> desktopScript)
        -- [style_ " grid-template-columns: repeat(auto-fill, minmax(600px, 1fr));", class_ "grid"]
        bgColor <- lift (readTVar vBgColor)
        with div_ [id_ "win-root", class_ $ "grow " <> into @Text bgColor] do
            with div_ [class_ "flex flex-col"] do
                let deskDiv = "border border-black rounded mx-2 my-3 w-6 grid align-center justify-center cursor-pointer"
                withTrigger "click" shellAppID "start-app" ["name" .= ProgramName "file-manager"] div_ [class_ deskDiv] do
                    with i_ [class_ "ri-computer-line"] mempty
                renderFileIcons shellAppID dir
                filesUploadButton shellAppID (getFileLoc dir Nothing)

        -- bottom bar
        with nav_ [id_ "display-menu", class_ "h-9 flex-none bg-slate-700 p-1 shadow w-full flex text-white z-50"] do
            with' div_ "grow" do
                with span_ [class_ "font-semibold mr-5 cursor-pointer", hxTrigger_ "click", wid_ shellAppID "wm-start", wsSend] ">>= start"
                with span_ [id_ "display-bar"] do
                    forM_ wids \wid -> with span_ [wid_ wid "bar"] mempty
            with' div_ "display-bar-right" do
                with span_ [id_ "display-tray", class_ "flex h-full w-full align-center justify-center"] do
                    appIDs <- Map.keys <$> lift (getApps apps)
                    forM_ appIDs \wid ->
                        with span_ [wid_ wid "tray"] mempty

        with div_ [id_ "reconnect_script"] mempty
        with div_ [id_ "background_script"] mempty

        with div_ [id_ "backstore"] do
            renderWindows shellAppID windows

welcomeWin :: Monad m => AppSet -> AppID -> HtmlT m ()
welcomeWin appSet wid = do
    with div_ [wid_ wid "w", class_ "grid grid-cols-1 divide-y"] do
        with div_ [class_ "p-2"] do
            "Welcome to "
            with span_ [class_ "font-bold"] "ButlerOS"
        div_ do
            "Press start!"
        with div_ [class_ "m-2"] do
            appSetHtml wid appSet

handleWinSwap :: WindowManager -> AppContext -> AppID -> ProgramName -> Maybe Value -> Maybe AppEvent -> ProcessIO ()
handleWinSwap wm ctx wid appName argv mEvent = do
    atomically do
        Map.lookup wid <$> getApps ctx.shared.apps >>= \case
            Just prevApp -> delApp ctx.shared prevApp
            Nothing -> pure ()
    delAppMemory wid
    mGuiApp <- launchApp ctx.shared.appSet appName argv ctx.shared wid
    case mGuiApp of
        Just guiApp -> swapWindow guiApp
        Nothing -> logInfo "unknown win-swap prog" ["v" .= appName]
  where
    swapWindow :: AppInstance -> ProcessIO ()
    swapWindow guiApp = do
        atomically $ addWindowApp wm guiApp
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
        sendsBinary ctx.shared.clients (encodeMessage (from shellAppID) (encodeJSON $ object body))

    broadcastSize :: (Int, Int) -> ProcessIO ()
    broadcastSize (x, y) =
        broadcastWinMessage ["w" .= wid, "ev" .= ("resize" :: Text), "x" .= x, "y" .= y]
