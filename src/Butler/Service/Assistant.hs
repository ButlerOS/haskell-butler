-- | This module contains the logic for assistants: personnal programs for helping users.
module Butler.Service.Assistant (withButlerSupervisor, butlerService) where

import Data.Map.Strict qualified as Map

import Butler.App
import Butler.AppID
import Butler.Core
import Butler.Core.Pipe
import Butler.Database
import Butler.Display.Client
import Butler.Display.GUI
import Butler.Display.Session
import Butler.Display.User
import Butler.Prelude

-- | The supervisor control the butler by assigning a process per session.
data ButlerSupervisor = ButlerSupervisor
    { processEnv :: ProcessEnv
    , running :: MVar (Map SessionID Butler)
    , db :: ButlerDB
    }

-- | The butler actor
data Butler = Butler
    { process :: Process
    , pipe :: Pipe ButlerEvent
    }

data ButlerEvent
    = ButlerJoined AppID DisplayClient
    | ButlerLeft DisplayClient
    | ButlerEvent GuiEvent
    deriving (Generic, ToJSON)

instance From ButlerEvent Session where
    from = \case
        ButlerJoined _ client -> client.session
        ButlerLeft client -> client.session
        ButlerEvent ev -> ev.client.session

-- | Create the 'ButlerSupervisor' to start the 'butlerService'.
withButlerSupervisor :: (ButlerSupervisor -> ProcessIO a) -> ProcessIO a
withButlerSupervisor cb = withDatabase "assistants" assistantDB \db -> do
    supervisor <- ButlerSupervisor <$> ask <*> newMVar mempty <*> pure (ButlerDB db)
    cb supervisor

-- | The service enables access to the butler.
butlerService :: ButlerSupervisor -> Service
butlerService = Service . defaultApp "assistant" . startButlerService

-- | The service connects client from a shared instance to their individual butler.
startButlerService :: ButlerSupervisor -> AppContext -> ProcessIO ()
startButlerService supervisor ctx = do
    let
        sendButler ev = do
            assistant <- getSessionButler supervisor (into @Session ev)
            writePipe assistant.pipe ev

    forever do
        atomically (readPipe ctx.pipe) >>= \case
            AppDisplay (UserJoined client) -> sendButler (ButlerJoined ctx.wid client)
            AppDisplay (UserLeft client) -> sendButler (ButlerLeft client)
            AppTrigger ev -> sendButler (ButlerEvent ev)
            ev -> logError "Unknown event" ["ev" .= ev]

-- | Find or create the session's butler.
getSessionButler :: ButlerSupervisor -> Session -> ProcessIO Butler
getSessionButler supervisor session = modifyMVar supervisor.running \running ->
    case Map.lookup session.sessionID running of
        Just x -> pure (running, x)
        Nothing -> do
            assistant <- newButler supervisor session
            pure (Map.insert session.sessionID assistant running, assistant)

-- | The butler manages 'ButlerApp' and a 'ButlerInstance' per session's client.
data ButlerState = ButlerState
    { apps :: TVar [ButlerAppInstance]
    , clients :: TVar (Map Endpoint ButlerInstance)
    }

newButlerState :: STM ButlerState
newButlerState = ButlerState <$> newTVar mempty <*> newTVar mempty

-- | Spawn a new butler.
newButler :: ButlerSupervisor -> Session -> ProcessIO Butler
newButler supervisor session = do
    pipe <- atomically newPipe
    username <- readTVarIO session.username
    process <-
        asProcess supervisor.processEnv do
            spawnProcess (from $ "butler-" <> into @Text username) do
                butlerState <- atomically newButlerState
                runButler supervisor.db session pipe butlerState
    pure Butler{process, pipe}

-- | A butler app / feature definition.
data ButlerApp = ButlerApp
    { session :: SessionID
    , program :: ProgramName
    , attrs :: TVar Value
    , createdAt :: UTCTime
    , updatedAt :: TVar UTCTime
    }

newButlerApp :: SessionID -> ProgramName -> Value -> ProcessIO ButlerApp
newButlerApp session program value = do
    createdAt <- liftIO getCurrentTime
    let updatedAt = createdAt
    ButlerApp session program <$> newTVarIO value <*> pure createdAt <*> newTVarIO updatedAt

-- | A butler app instance.
data ButlerAppInstance = ButlerAppInstance
    { handleEvent :: ButlerInstance -> ButlerEvent -> ProcessIO ()
    , render :: AppID -> HtmlT STM ()
    }

startButlerApp :: ButlerDB -> ButlerState -> ButlerApp -> ProcessIO (Maybe ButlerAppInstance)
startButlerApp db state app = do
    mApp <- case app.program of
        "welcome" -> Just <$> welcomeButler db app
        _ -> pure Nothing
    case mApp of
        Nothing -> logError "Invalid app" ["app" .= app.program]
        Just appInstance -> do
            atomically $ modifyTVar' state.apps (appInstance :)
    pure mApp

-- | A direct butler connection.
data ButlerInstance = ButlerInstance
    { wid :: AppID
    , client :: DisplayClient
    , status :: TVar ButlerStatus
    , chat :: TVar (Maybe (Text, HtmlT STM ()))
    }

data ButlerStatus = Hidden | Alerting | Interacting

newButlerInstance :: AppID -> DisplayClient -> STM ButlerInstance
newButlerInstance wid client = ButlerInstance wid client <$> newTVar Hidden <*> newTVar Nothing

runButler :: ButlerDB -> Session -> Pipe ButlerEvent -> ButlerState -> ProcessIO ()
runButler db session pipe state = do
    -- Refresh the butler memory
    savedApps <- loadButlerApps db session.sessionID
    case savedApps of
        [] -> do
            -- This is a new user, start the welcome app.
            void . startButlerApp db state =<< newButlerApp session.sessionID "welcome" Null
        xs -> traverse_ (startButlerApp db state) xs

    -- The UI
    let uiClass = "absolute bottom-11 right-2 border p-3 bg-orange-100 max-w-lg text-stone-700"
        convoUI :: Maybe Session -> HtmlT STM () -> HtmlT STM ()
        convoUI mSession msg = do
            with div_ [class_ "flex flex-row"] do
                case mSession of
                    Nothing -> with div_ [class_ "ri-aliens-line relative -left-1"] mempty
                    Just _ -> do
                        with div_ [class_ "relative -left-1"] do
                            userIcon =<< lift (readTVar session.username)
                with div_ [class_ "flex flex-col"] msg

    let renderChatUI, hideChatUI, trayUI :: ButlerInstance -> HtmlT STM ()
        renderChatUI ButlerInstance{wid, chat, client} = with div_ [wid_ wid "assistant", class_ uiClass] do
            convoUI Nothing do
                apps <- lift (readTVar state.apps)
                traverse_ (\app -> app.render wid) apps
            br_ []
            with div_ [wid_ wid "chat"] do
                lift (readTVar chat) >>= \case
                    Just (prompt, resp) -> do
                        convoUI (Just client.session) (toHtml prompt)
                        convoUI Nothing resp
                        withTrigger_ "click" wid "prompt" button_ [] "x"
                    Nothing -> pure ()
                withTrigger_ "" wid "prompt" (input_ []) [type_ "text", placeholder_ "Prompt...", name_ "value", class_ inputClass]

        hideChatUI ButlerInstance{wid} = with div_ [wid_ wid "assistant"] mempty

        trayUI ButlerInstance{wid} = with div_ [wid_ wid "tray"] do
            withTrigger_ "click" wid "toggle-assistant" span_ [class_ "inline-block m-auto ml-1 h-3 w-3 center rounded-full opacity-75 cursor-pointer bg-sky-300"] mempty

        mountUI :: ButlerInstance -> ProcessIO ()
        mountUI i@ButlerInstance{wid, client, status} = atomically $ sendHtml client $ with div_ [wid_ wid "tray"] do
            trayUI i
            lift (readTVar status) >>= \case
                Hidden -> with div_ [wid_ wid "assistant"] mempty
                Alerting -> with div_ [wid_ wid "assistant"] mempty
                Interacting -> renderChatUI i

        handlePrompt _butlerInstance _prompt = pure "Sorry, I don't understand"

        handleEvent ev butlerInstance = do
            apps <- atomically (readTVar state.apps)
            forM_ apps \app -> app.handleEvent butlerInstance (ButlerEvent ev)
            case ev.trigger of
                "toggle-assistant" -> do
                    newUI <- atomically do
                        prevStatus <- readTVar butlerInstance.status
                        newStatus <- case prevStatus of
                            Interacting -> pure Hidden
                            _ -> do
                                writeTVar butlerInstance.chat Nothing
                                pure Interacting

                        writeTVar butlerInstance.status newStatus
                        pure $ case newStatus of
                            Hidden -> hideChatUI
                            Alerting -> hideChatUI
                            Interacting -> renderChatUI
                    atomically $ sendHtml butlerInstance.client $ newUI butlerInstance
                "prompt" -> do
                    case ev.body ^? key "value" . _String of
                        Just prompt -> do
                            resp <- handlePrompt butlerInstance prompt
                            atomically $ writeTVar butlerInstance.chat $ Just (prompt, resp)
                        Nothing -> atomically $ writeTVar butlerInstance.chat Nothing
                    atomically $ sendHtml butlerInstance.client $ renderChatUI butlerInstance
                _ -> logError "Unknown ev" ["ev" .= ev]

    -- The event loop
    forever do
        atomically (readPipe pipe) >>= \case
            ButlerJoined wid client -> do
                butlerInstance <- atomically (newButlerInstance wid client)
                atomically $ modifyTVar' state.clients $ Map.insert client.endpoint butlerInstance
                apps <- atomically (readTVar state.apps)
                forM_ apps \app -> app.handleEvent butlerInstance (ButlerJoined wid client)
                mountUI butlerInstance
            ButlerLeft client ->
                atomically (Map.lookup client.endpoint <$> readTVar state.clients) >>= \case
                    Just butlerInstance -> do
                        apps <- atomically (readTVar state.apps)
                        forM_ apps \app -> app.handleEvent butlerInstance (ButlerLeft client)
                        atomically $ modifyTVar' state.clients $ Map.delete client.endpoint
                    Nothing -> logError "Unknown client" ["ev" .= client]
            ButlerEvent ev ->
                atomically (Map.lookup ev.client.endpoint <$> readTVar state.clients) >>= \case
                    Just butlerInstance -> handleEvent ev butlerInstance
                    Nothing -> logError "Unknown client" ["ev" .= ev]

-- | The welcome assistant introduce butler to the user.
data WelcomeState = WNew | WToggled | WDone
    deriving (Generic, FromJSON, ToJSON)

welcomeButler :: ButlerDB -> ButlerApp -> ProcessIO ButlerAppInstance
welcomeButler db app = do
    -- load state var
    welcomeState <- atomically do
        attrs <- readTVar app.attrs
        newTVar $ case fromJSON @Int attrs of
            Success version | version >= 0 -> WDone
            _ -> WNew
    -- update state and attrs
    let updateState newState = do
            atomically $ writeTVar welcomeState newState
            updateButlerApp db app (toJSON @Int 0)

    let render _wid =
            lift (readTVar welcomeState) >>= \case
                WNew ->
                    div_ do
                        p_ "Welcome to ButlerOS"
                        p_ "I am your personnal assistant!"
                        br_ []
                        p_ "Click twice on the blue dot at the bottom right of your screen."
                WToggled ->
                    div_ do
                        p_ "Good, now you know how to invoke me"
                        br_ []
                        p_ do
                            "I'm currently operating the "
                            b_ "welcome"
                            " app. I'll stop it after you close this window."
                        -- br_ []
                        -- renderAppList wid
                        br_ []
                        p_ "You can use the text input below, but I'm not very good at it yet :)"
                WDone -> "What can I do for you?"
        handleEvent butlerInstance = \case
            ButlerJoined{} -> do
                readTVarIO welcomeState >>= \case
                    WNew -> atomically $ writeTVar butlerInstance.status Interacting
                    _ -> pure ()
            ButlerEvent ev ->
                case ev.trigger of
                    "toggle-assistant" -> do
                        newState <- atomically do
                            status <- readTVar butlerInstance.status
                            modifyTVar' welcomeState \case
                                WNew -> WToggled
                                WToggled -> case status of
                                    Interacting -> WDone
                                    _ -> WToggled
                                WDone -> WDone
                            readTVar welcomeState
                        case newState of
                            WDone -> updateState WDone
                            _ -> pure ()
                    _ -> pure ()
            _ -> pure ()

    pure $ ButlerAppInstance{handleEvent, render}

{-
renderAppList :: AppID -> HtmlT STM ()
renderAppList wid = do
    p_ "What can I do for you?"
    with ul_ [class_ "list-disc"] do
        li_ do
            withTrigger "click" wid "start-app" ["app" .= ("welcome" :: Text)] b_ [class_ "cursor-pointer"] "restart welcome program"
        li_ do
            withTrigger "click" wid "start-app" ["app" .= ("tips" :: Text)] b_ [class_ "cursor-pointer"] "tips of the day"
        li_ do
            withTrigger "click" wid "start-app" ["app" .= ("recover" :: Text)] b_ [class_ "cursor-pointer"] "show recovery link"
-}

-- | The database stores the assistant memory.
newtype ButlerDB = ButlerDB Database

assistantDB :: DatabaseMigration
assistantDB = dbSimpleCreate "apps" "session TEXT, program TEXT, attrs BLOB, createdAt DATE, updatedAt DATE"

loadButlerApps :: ButlerDB -> SessionID -> ProcessIO [ButlerApp]
loadButlerApps (ButlerDB db) session =
    traverse mkEvent
        =<< dbQuery db "SELECT program,attrs,createdAt,updatedAt FROM apps WHERE session = :session" [":session" := session]
  where
    mkEvent (from @Text -> program, from @ValueB -> attrs, createdAt, updatedAt) =
        ButlerApp session program <$> newTVarIO attrs <*> pure createdAt <*> newTVarIO updatedAt

updateButlerApp :: ButlerDB -> ButlerApp -> Value -> ProcessIO ()
updateButlerApp (ButlerDB db) app attrs = do
    now <- liftIO getCurrentTime
    atomically do
        writeTVar app.updatedAt now
        writeTVar app.attrs attrs
    let
        sqlNames = [":attrs" := into @ValueB attrs, ":updatedAt" := now, ":program" := into @Text app.program, ":session" := app.session]
    updateCount <-
        dbUpdate
            db
            "UPDATE apps SET attrs = :attrs, updatedAt = :updatedAt WHERE program = :program AND session = :session"
            sqlNames
    when (updateCount == 0) do
        dbExecute
            db
            "INSERT into apps (session,program,attrs,createdAt,updatedAt) VALUES (:session, :program, :attrs, :createdAt, :updatedAt)"
            ((":createdAt" := app.createdAt) : sqlNames)
