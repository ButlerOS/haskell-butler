{- | This module contains the logic to serve multiple 'Desktop'.
The 'Lobby' act as a router to dispatch user based on the workspace path.
-}
module Butler.Lobby (lobbyProgram) where

import Data.List qualified
import Data.Map.Strict qualified as Map
import Data.UUID qualified as UUID
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
import Butler.Display.User
import Butler.Display.WebSocket

import Butler.App.Chat
import Butler.Service.Assistant

data Lobby = Lobby
    { desktops :: MVar (Map Workspace DesktopStatus)
    , desktopsList :: MemoryVar [Workspace]
    , update :: BroadcastChan ()
    }

data DesktopStatus = DesktopOffline | DesktopRunning DesktopInstance

isDesktopStatusPublic :: DesktopStatus -> Bool
isDesktopStatusPublic = \case
    DesktopOffline -> True -- Only public desktop are saved and can be offline
    DesktopRunning desktopInstance -> isNothing desktopInstance.owner

data DesktopInstance = DesktopInstance
    { owner :: Maybe Session
    , desktop :: Desktop
    , onClient :: (ProcessEnv, DisplayEvent -> ProcessIO ())
    }

newLobby :: ProcessIO Lobby
newLobby = do
    (savedList, desktopsList) <- newProcessMemory "desktops.bin" (pure mempty)
    let initialMap = Map.fromList $ map (\name -> (name, DesktopOffline)) savedList
    Lobby <$> newMVar initialMap <*> pure desktopsList <*> atomically newBroadcastChan

lobbyProgram :: ButlerSupervisor -> AppSet -> ChatServer -> Display -> ProcessIO OnClient
lobbyProgram bs appSet chat display = do
    dm <- newLobby
    displayProcess <- getSelfProcess
    dmProcess <- spawnProcess "lobby" do
        void $ waitProcess displayProcess
    rootOS <- asks os
    let dmEnv = ProcessEnv rootOS dmProcess
    let dApp = fromMaybe (error "Missing desktop app") $ lookupAppSet "desktop" appSet

    let waitingDesktop :: ProcessIO DesktopInstance
        waitingDesktop = do
            env <- ask
            pure $
                DesktopInstance
                    Nothing
                    (Desktop mempty mempty undefined)
                    ( env
                    , \case
                        UserConnected "htmx" client -> do
                            spawnThread_ (sendThread client)
                            atomically $ sendHtml client (with div_ [id_ "display-wins"] (splashHtml "Workspace is not available!"))
                            sleep 600_000
                        _ -> pure ()
                    )

    let getDesktop :: Either Session Workspace -> ProcessIO DesktopInstance
        getDesktop eName = modifyMVar dm.desktops $ \wss -> case Map.lookup name wss of
            Just (DesktopRunning desktop) -> pure (wss, desktop)
            mDesktopStatus
                | isNothing owner && isJust (UUID.fromText (from name)) ->
                    (wss,) <$> waitingDesktop
                | otherwise -> do
                    let desktopID = "desktop-" <> into @StorageAddress name
                    (shared, shellInstance) <- chroot desktopID do
                        startShellApp appSet (from name <> "-") dApp display

                    when (isNothing owner && isNothing mDesktopStatus) do
                        atomically $ modifyMemoryVar dm.desktopsList (name :)

                    desktop <- fromMaybe (error "Desktop call failed") <$> appCall shellInstance

                    let desktopInstance = DesktopInstance owner desktop (shared.processEnv, shellHandler bs owner desktop shared)
                    atomically $ broadcast dm.update ()
                    pure (Map.insert name (DesktopRunning desktopInstance) wss, desktopInstance)
          where
            (name, owner) = case eName of
                Left session -> (Workspace (from session.sessionID), Just session)
                Right ws -> (ws, Nothing)

    pure \session -> \case
        Workspace "" -> (.onClient) <$> getDesktop (Left session)
        Workspace "_" -> pure (dmEnv, lobbyHandler dm chat)
        name -> (.onClient) <$> getDesktop (Right name)

shellHandler :: ButlerSupervisor -> Maybe Session -> Desktop -> AppSharedContext -> DisplayEvent -> ProcessIO ()
shellHandler butlerSupervisor mOwner desktop shared event = case event of
    UserConnected "htmx" client -> do
        -- Setup client
        spawnThread_ (sendThread client)

        -- Verify ACL
        case mOwner of
            Just owner | owner.sessionID /= client.session.sessionID -> do
                let isDenied = isNothing . Data.List.find (\session -> session.sessionID == client.session.sessionID)
                whenM (isDenied <$> readTVarIO desktop.acls) do
                    atomically $ sendHtml client (with div_ [id_ "display-wins"] (splashHtml "One moment please..."))
                    -- Request permission
                    ownerButler <- getSessionButler shared.display butlerSupervisor owner
                    tmReply <- newEmptyTMVarIO
                    writePipe ownerButler.pipe (ButlerSync (ButlerSyncEvent client "desktop-access-request" (SyncEvent tmReply)))
                    -- Wait for result...
                    atomically (fromDynamic <$> takeTMVar tmReply) >>= \case
                        Just True -> do
                            atomically $ modifyTVar' desktop.acls (client.session :)
                        _ -> do
                            atomically $ sendHtml client (with div_ [id_ "display-wins"] (splashHtml "Access denied"))
                            sleep 1_000
                            error "stop"
            _ -> pure () -- This is a public desktop, or the client is the owner

        -- Mount the shell UI
        atomically $ sendHtml client desktop.mountUI
        atomically $ addClient shared.clients client

        -- Mount the running apps UI
        appInstances <- atomically (getApps shared.apps)
        forM_ appInstances \appInstance -> writePipe appInstance.pipe (AppDisplay (UserJoined client))

        -- Handle client inputs
        forever do
            dataMessage <- recvData client
            case eventFromMessage client dataMessage of
                Nothing -> logError "Unknown data" ["ev" .= LBSLog (into @LByteString dataMessage)]
                Just (wid, ae) ->
                    (Map.lookup wid <$> atomically (getApps shared.apps)) >>= \case
                        Nothing -> logError "Unknown wid" ["wid" .= wid, "ev" .= ae]
                        Just appInstance -> writePipe appInstance.pipe ae
    UserDisconnected "htmx" client -> do
        -- TODO: check ACLs
        atomically $ delClient shared.clients client
        apps <- atomically (getApps shared.apps)
        forM_ apps \app -> writePipe app.pipe (AppDisplay (UserLeft client))
    UserConnected chan _ -> do
        -- TODO: check ACLs
        forwardExtraHandler chan
    UserDisconnected chan _ -> do
        -- TODO: check ACLs
        forwardExtraHandler chan
  where
    forwardExtraHandler chan =
        Map.lookup chan <$> readTVarIO shared.extraHandlers >>= \case
            Just hdl -> hdl event
            Nothing -> logError "Unknown chan" ["chan" .= chan]

lobbyHandler :: Lobby -> ChatServer -> DisplayEvent -> ProcessIO ()
lobbyHandler dm chat = \case
    UserConnected "htmx" client -> do
        spawnThread_ (sendThread client)
        atomically $ sendHtml client (with div_ [id_ "display-wins"] (splashHtml "Loading..."))
        sleep 100
        atomically $ sendHtml client (with div_ [id_ "display-wins"] (splashHtml "Getting workspaces..."))
        wss <- readMVar dm.desktops
        chatChan <- atomically (newChatReader chat)
        sleep 60
        let chatWin = shellAppID
        atomically $ sendHtml client (lobbyHtml wss (renderChat chatWin chat client) client)
        spawnThread_ $ forever do
            ev <- atomically (readTChan chatChan)
            atomically $ sendHtml client (updateChat chatWin client ev)

        updateChan <- atomically (newReaderChan dm.update)
        spawnThread_ $ forever do
            () <- atomically (readTChan updateChan)
            newWss <- readMVar dm.desktops
            atomically $ sendHtml client (lobbyHtml newWss (renderChat chatWin chat client) client)
            logInfo "Updating ws..." []

        forever do
            lbs <- into <$> recvData client
            case decodeJSON @HtmxEvent lbs of
                Nothing -> logError "Unknown ev" ["ev" .= LBSLog lbs]
                Just htmxEvent -> handleLobbyEvents dm chat client (TriggerName htmxEvent.trigger) htmxEvent.body
    _ -> pure ()

lobbyWsListHtml :: Map Workspace DesktopStatus -> DisplayClient -> HtmlT STM ()
lobbyWsListHtml wss client = do
    let (public, private) = Data.List.partition (isDesktopStatusPublic . snd) (Map.toList wss)
    with div_ [id_ "ws-list", class_ "grid auto-cols-auto w-96"] do
        with div_ [class_ "p-2 mt-4 bg-stone-300 rounded relative"] do
            with div_ [class_ "text-center"] do
                with
                    button_
                    [hxTrigger_ "click", wsSend, id_ "enter-ws", encodeVal [("ws", "")], class_ btn]
                    "Enter Personnal Workspace"
            forM_ private \(ws, desktopStatus) -> case desktopStatus of
                DesktopRunning desktopInstance -> case desktopInstance.owner of
                    Just owner | owner.sessionID /= client.session.sessionID ->
                        with button_ [hxTrigger_ "click", wsSend, id_ "enter-ws", encodeVal [("ws", toJSON ws)], class_ btn] do
                            userIcon =<< lift (readTVar owner.username)
                    _ -> pure ()
                _ -> pure ()
        forM_ public $ \(ws, desktopStatus) -> do
            with div_ [class_ "p-2 mt-4 bg-stone-300 rounded relative"] do
                with div_ [class_ "flex justify-end absolute bottom-0 right-0"] do
                    with
                        i_
                        [id_ "delete-ws", encodeVal [("ws", toJSON ws)], class_ "px-4 py-2 ri-focus-3-fill text-red-500", hxTrigger_ "click", wsSend]
                        mempty
                    with
                        button_
                        [hxTrigger_ "click", wsSend, id_ "enter-ws", encodeVal [("ws", toJSON ws)], class_ btn]
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
                        DesktopRunning desktopInstance -> desktopInstance.desktop.thumbnail

btn :: Text
btn = "rounded-none px-4 py-2 font-semibold text-sm bg-sky-500 text-white rounded-none shadow-sm"

lobbyHtml :: Map Workspace DesktopStatus -> HtmlT STM () -> DisplayClient -> HtmlT STM ()
lobbyHtml wss chat client =
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
                        lobbyWsListHtml wss client
                        with form_ [wsSend, hxTrigger_ "submit", class_ "mt-4 p-2 flex flex-col bg-stone-300 rounded w-96 relative", id_ "new-ws"] do
                            with (input_ mempty) [name_ "ws", type_ "text", placeholder_ "Workspace name"]
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
                        DesktopRunning desktopInstance -> void $ killProcess (fst $ desktopInstance.onClient).process.pid
                        DesktopOffline -> pure ()
                    let newWss = Map.delete ws wss
                    atomically $ sendHtml client (lobbyWsListHtml newWss client)
                    atomically $ modifyMemoryVar dm.desktopsList (filter (/= ws))
                    atomically $ broadcast dm.update ()
                    pure newWss
                Nothing -> do
                    logError "unknown ws" ["ws" .= ws]
                    pure wss
            "enter-ws" -> do
                atomically $ sendHtml client do
                    with div_ [id_ "display-wins"] do
                        script_ $ "window.location.pathname = \"/" <> wsTxt <> "\""
            "new-ws" | ws /= "" -> do
                atomically $ sendHtml client do
                    with div_ [id_ "display-wins"] do
                        script_ $ "window.location.pathname = \"/" <> wsTxt <> "\""
            _ -> logError "unknown welcome event" ["ev" .= ev]
    Nothing -> case ev ^? key "message" . _String of
        Just msg -> atomically do
            username <- readTVar client.session.username
            addUserMessage chat (MkMessage username msg)
        Nothing -> logError "missing ws" ["ev" .= ev]
