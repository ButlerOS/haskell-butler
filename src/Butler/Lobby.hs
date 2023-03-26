{- | This module contains the logic to serve multiple 'Desktop'.
The 'Lobby' act as a router to dispatch user based on the workspace path.
-}
module Butler.Lobby (lobbyProgram) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Lucid
import Lucid.Htmx

import Butler
import Butler.App
import Butler.App.Desktop
import Butler.AppID
import Butler.Core
import Butler.Core.Logger
import Butler.Display
import Butler.Display.Session
import Butler.Display.WebSocket

import Butler.App.Chat

data Lobby = Lobby
    { desktops :: MVar (Map Workspace DesktopStatus)
    , desktopsList :: MemoryVar [Workspace]
    }

data DesktopStatus = DesktopOffline | DesktopRunning DesktopInstance

type DesktopInstance = (Desktop, (ProcessEnv, DisplayEvent -> ProcessIO ()))

newLobby :: ProcessIO Lobby
newLobby = do
    (savedList, desktopsList) <- newProcessMemory "desktops.bin" (pure mempty)
    let initialMap = Map.fromList $ map (\name -> (name, DesktopOffline)) savedList
    Lobby <$> newMVar initialMap <*> pure desktopsList

lobbyProgram :: AppSet -> [Service] -> ChatServer -> Display -> ProcessIO OnClient
lobbyProgram appSet services chat display = do
    dm <- newLobby
    displayProcess <- getSelfProcess
    dmProcess <- spawnProcess "lobby" do
        void $ waitProcess displayProcess
    rootOS <- asks os
    let dmEnv = ProcessEnv rootOS dmProcess

    let getDesktop :: Workspace -> ProcessIO DesktopInstance
        getDesktop name = modifyMVar dm.desktops $ \wss -> case Map.lookup name wss of
            Just (DesktopRunning desktop) -> pure (wss, desktop)
            mDesktopStatus -> do
                let desktopID = "desktop-" <> into @StorageAddress name
                (shared, shellInstance) <- chroot desktopID $ startShellApp appSet "desktop-" (desktopApp services & #name .~ via @Text name) display

                when (isNothing mDesktopStatus) do
                    atomically $ modifyMemoryVar dm.desktopsList (name :)

                desktop <- fromMaybe (error "Desktop call failed") <$> appCall shellInstance

                let desktopInstance = (desktop, (shared.processEnv, shellHandler desktop shared))
                pure (Map.insert name (DesktopRunning desktopInstance) wss, desktopInstance)

    pure \_ -> \case
        Workspace "" -> pure (dmEnv, lobbyHandler dm chat)
        name -> snd <$> getDesktop name

shellHandler :: Desktop -> AppSharedContext -> DisplayEvent -> ProcessIO ()
shellHandler desktop shared event = case event of
    UserConnected "htmx" client -> do
        -- No need to spawn a ping thread because the desktop update the status periodically.
        spawnThread_ (sendThread client)
        atomically $ sendHtml client desktop.mountUI
        atomically $ addClient shared.clients client

        appInstances <- atomically (getApps shared.apps)
        forM_ appInstances \appInstance -> writePipe appInstance.pipe (AppDisplay (UserJoined client))

        forever do
            dataMessage <- recvData client
            case eventFromMessage client dataMessage of
                Nothing -> logError "Unknown data" ["ev" .= LBSLog (into @LByteString dataMessage)]
                Just (wid, ae) ->
                    (Map.lookup wid <$> atomically (getApps shared.apps)) >>= \case
                        Nothing -> logError "Unknown wid" ["wid" .= wid, "ev" .= ae]
                        Just appInstance -> writePipe appInstance.pipe ae
    UserDisconnected "htmx" client -> do
        atomically $ delClient shared.clients client
        apps <- atomically (getApps shared.apps)
        forM_ apps \app -> writePipe app.pipe (AppDisplay (UserLeft client))
    UserConnected chan _ -> forwardExtraHandler chan
    UserDisconnected chan _ -> forwardExtraHandler chan
  where
    forwardExtraHandler chan =
        Map.lookup chan <$> readTVarIO shared.extraHandlers >>= \case
            Just hdl -> hdl event
            Nothing -> logError "Unknown chan" ["chan" .= chan]

lobbyHandler :: Lobby -> ChatServer -> DisplayEvent -> ProcessIO ()
lobbyHandler dm chat = \case
    UserConnected "htmx" client -> do
        spawnThread_ (pingThread client)
        spawnThread_ (sendThread client)
        atomically $ sendHtml client (with div_ [id_ "display-wins"] (splashHtml "Loading..."))
        sleep 100
        atomically $ sendHtml client (with div_ [id_ "display-wins"] (splashHtml "Getting workspaces..."))
        wss <- readMVar dm.desktops
        chatChan <- atomically (newChatReader chat)
        sleep 60
        let chatWin = shellAppID
        atomically $ sendHtml client (lobbyHtml wss (renderChat chatWin chat client))
        spawnThread_ $ forever do
            ev <- atomically (readTChan chatChan)
            atomically $ sendHtml client (updateChat chatWin client ev)
        forever do
            lbs <- into <$> recvData client
            case decodeJSON @HtmxEvent lbs of
                Nothing -> logError "Unknown ev" ["ev" .= LBSLog lbs]
                Just htmxEvent -> handleLobbyEvents dm chat client (TriggerName htmxEvent.trigger) htmxEvent.body
    _ -> pure ()

lobbyWsListHtml :: Map Workspace DesktopStatus -> HtmlT STM ()
lobbyWsListHtml wss =
    with div_ [id_ "ws-list", class_ "grid auto-cols-auto w-96"] do
        forM_ (Map.toList wss) $ \(ws, desktopStatus) -> do
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
                        let attr :: Text -> HtmlT STM () -> HtmlT STM ()
                            attr k v =
                                tr_ do
                                    with td_ [class_ "text-right pr-1"] do
                                        toHtml k
                                        ":"
                                    with td_ [class_ "font-medium"] do
                                        v
                        attr "name" (toHtml ws)
                        case desktopStatus of
                            DesktopOffline -> "offline"
                            DesktopRunning (desktop, _) -> desktop.thumbnail

btn :: Text
btn = "rounded-none px-4 py-2 font-semibold text-sm bg-sky-500 text-white rounded-none shadow-sm"

lobbyHtml :: Map Workspace DesktopStatus -> HtmlT STM () -> HtmlT STM ()
lobbyHtml wss chat =
    with div_ [id_ "display-wins"] do
        script_ "htmx.find('#display-ws').setAttribute('ws-connect', '/ws/htmx?reconnect=true')"
        splashHtml do
            with div_ [class_ "flex"] do
                with div_ [class_ "flex flex-col place-content-center pr-3"] do
                    with div_ [class_ "flex flex-col place-content-center mx-auto"] do
                        with div_ [class_ "items-center justify-center whitespace-nowrap"] do
                            "Welcome to "
                            with span_ [class_ "font-bold"] "ButlerOS"
                        with div_ [class_ "items-center justify-center grid"] do
                            img_ [src_ "/xstatic/favicon.ico"]
                    with div_ [class_ ""] do
                        chat
                with div_ [class_ "border border-2 mr-3"] mempty
                with div_ [class_ "flex-grow"] do
                    with div_ [class_ "flex flex-col place-items-center"] do
                        lobbyWsListHtml wss
                        with form_ [wsSend, hxTrigger_ "submit", class_ "mt-4 p-2 flex flex-col bg-stone-300 rounded w-96 relative", id_ "new-ws"] do
                            with (input_ mempty) [name_ "ws", type_ "text", placeholder_ "Workspace name"]
                            with select_ [name_ "flavor"] do
                                option_ "localhost"
                                option_ "quay.io/org/toolbox"
                            with (input_ mempty) [type_ "submit", class_ btn, value_ "create"]

handleLobbyEvents :: Lobby -> ChatServer -> DisplayClient -> TriggerName -> Value -> ProcessIO ()
handleLobbyEvents dm chat client trigger ev = case ev ^? key "ws" . _String of
    Just wsTxt -> do
        let ws = Workspace wsTxt
        logInfo "got welcome event" ["ws" .= ws, "trigger" .= trigger]
        case trigger of
            "delete-ws" -> modifyMVar_ dm.desktops $ \wss -> case Map.lookup ws wss of
                Just desktopStatus -> do
                    case desktopStatus of
                        DesktopRunning (_, (env, _)) -> void $ killProcess env.process.pid
                        DesktopOffline -> pure ()
                    let newWss = Map.delete ws wss
                    atomically $ sendHtml client (lobbyWsListHtml newWss)
                    atomically $ modifyMemoryVar dm.desktopsList (filter (/= ws))
                    pure newWss
                Nothing -> do
                    logError "unknown ws" ["ws" .= ws]
                    pure wss
            "enter-ws" -> do
                atomically $ sendHtml client do
                    with div_ [id_ "display-wins"] do
                        script_ $ "window.location.pathname = \"/" <> wsTxt <> "\""
            "new-ws" -> do
                let cleanWS = Text.takeWhile (\c -> isAlphaNum c || c == '-') wsTxt
                case cleanWS of
                    "" -> logError "invalid ws" ["ws" .= cleanWS]
                    _ -> do
                        atomically $ sendHtml client do
                            with div_ [id_ "display-wins"] do
                                script_ $ "window.location.pathname = \"/" <> cleanWS <> "\""
            _ -> logError "unknown welcome event" ["ev" .= ev]
    Nothing -> case ev ^? key "message" . _String of
        Just msg -> atomically do
            username <- readTVar client.session.username
            addUserMessage chat (MkMessage username msg)
        Nothing -> logError "missing ws" ["ev" .= ev]
