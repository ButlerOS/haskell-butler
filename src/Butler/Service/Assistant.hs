-- | This module contains the logic for assistants: personnal programs for helping users.
module Butler.Service.Assistant (
    ButlerSupervisor,
    Butler (..),
    ButlerEvent (..),
    ButlerSyncEvent (..),
    withButlerSupervisor,
    butlerService,
    getSessionButler,
) where

import Data.List (find)
import Data.Map.Strict qualified as Map

import Butler.App
import Butler.App.QRTest (qrEncode)
import Butler.AppID
import Butler.Core
import Butler.Core.Network (ServerName)
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
    | ButlerSync ButlerSyncEvent
    deriving (Generic, ToJSON)

data ButlerSyncEvent = ButlerSyncEvent
    { client :: DisplayClient
    , trigger :: TriggerName
    , sync :: SyncEvent
    }
    deriving (Generic, ToJSON)

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
        sendButler session ev = do
            assistant <- getSessionButler ctx.shared.display supervisor session
            writePipe assistant.pipe ev

    forever do
        atomically (readPipe ctx.pipe) >>= \case
            AppDisplay (UserJoined client) -> sendButler client.session (ButlerJoined ctx.wid client)
            AppDisplay (UserLeft client) -> sendButler client.session (ButlerLeft client)
            AppTrigger ev -> sendButler ev.client.session (ButlerEvent ev)
            ev -> logError "Unknown event" ["ev" .= ev]

-- | Find or create the session's butler.
getSessionButler :: Display -> ButlerSupervisor -> Session -> ProcessIO Butler
getSessionButler display supervisor session = modifyMVar supervisor.running \running ->
    case Map.lookup session.sessionID running of
        Just x -> pure (running, x)
        Nothing -> do
            assistant <- newButler display supervisor session
            pure (Map.insert session.sessionID assistant running, assistant)

-- | The butler manages 'ButlerApp' and a 'ButlerInstance' per session's client.
data ButlerState = ButlerState
    { session :: Session
    , apps :: TVar [ButlerAppInstance]
    , clients :: TVar (Map Endpoint ButlerInstance)
    }

newButlerState :: Session -> STM ButlerState
newButlerState session = ButlerState session <$> newTVar mempty <*> newTVar mempty

-- | Spawn a new butler.
newButler :: Display -> ButlerSupervisor -> Session -> ProcessIO Butler
newButler display supervisor session = do
    pipe <- atomically newPipe
    username <- readTVarIO session.username
    process <-
        asProcess supervisor.processEnv do
            spawnProcess (from $ "butler-" <> into @Text username) do
                butlerState <- atomically (newButlerState session)
                runButler display supervisor.db session pipe butlerState
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
    { app :: ButlerApp
    , handleRequest :: ButlerSyncEvent -> ProcessIO ()
    , handleEvent :: ButlerInstance -> ButlerEvent -> ProcessIO ()
    , render :: AppID -> HtmlT STM ()
    }

startButlerApp :: ButlerDB -> ButlerState -> ButlerApp -> ProcessIO (Maybe ButlerAppInstance)
startButlerApp db state app = do
    mApp <- case app.program of
        "welcome" -> Just <$> welcomeButler db state app
        "authorizer" -> Just <$> authorizerButler state app
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

runButler :: Display -> ButlerDB -> Session -> Pipe ButlerEvent -> ButlerState -> ProcessIO ()
runButler display db session pipe state = do
    -- Refresh the butler memory
    savedApps <- loadButlerApps db session.sessionID
    case savedApps of
        [] -> do
            -- This is a new user, start the welcome app.
            void . startButlerApp db state =<< newButlerApp session.sessionID "welcome" Null
        xs -> traverse_ (startButlerApp db state) xs

    let getApp :: ProgramName -> STM (Maybe ButlerAppInstance)
        getApp name = find (\appInstance -> appInstance.app.program == name) <$> readTVar state.apps

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
            withTrigger_ "click" wid "toggle-assistant" span_ [class_ "px-1 ml-1 cursor-pointer"] do
                with span_ [class_ "inline-block m-auto h-3 w-3 rounded-full opacity-75 bg-sky-300"] mempty

        mountUI :: ButlerInstance -> ProcessIO ()
        mountUI i@ButlerInstance{wid, client, status} = atomically $ sendHtml client $ with div_ [wid_ wid "tray"] do
            trayUI i
            lift (readTVar status) >>= \case
                Hidden -> with div_ [wid_ wid "assistant"] mempty
                Alerting -> with div_ [wid_ wid "assistant"] mempty
                Interacting -> renderChatUI i

        recoveryLink :: ServerName -> RecoveryID -> HtmlT STM ()
        recoveryLink srv recoveryID = do
            "Use this link to recover your session: "
            with a_ [href_ recoveryURL] (qrEncode recoveryURL)
          where
            recoveryURL = "https://" <> from srv <> "/_recovery?recover=" <> from recoveryID

        handlePrompt :: ButlerInstance -> Text -> ProcessIO (HtmlT STM ())
        handlePrompt butlerInstance = \case
            "disconnect me" -> pure do
                span_ "Bye!"
                script_ "window.location.pathname = \"/_\""
            "make recovery link" ->
                readTVarIO session.recover >>= \case
                    Nothing -> do
                        recoveryID <- getOrCreateRecover display.sessions session
                        pure $ recoveryLink butlerInstance.client.server recoveryID
                    Just _ -> pure "Recovery link already exist!"
            "show recovery link" ->
                readTVarIO session.recover >>= \case
                    Nothing -> pure "Recovery link is missing!"
                    Just recoveryID -> pure $ recoveryLink butlerInstance.client.server recoveryID
            "delete recovery link" -> do
                deleteRecover display.sessions session
                pure "Done!"
            _ -> pure "Sorry, I don't understand"

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
                _ -> pure ()

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
            ButlerSync ev -> case ev.trigger of
                "desktop-access-request" -> do
                    -- Check if authorizer is running
                    mAuthorizer <-
                        atomically (getApp "authorizer") >>= \case
                            Nothing -> startButlerApp db state =<< newButlerApp session.sessionID "authorizer" Null
                            x -> pure x
                    case mAuthorizer of
                        Just authorizer -> authorizer.handleRequest ev
                        Nothing -> logError "Couldn't start authorizer" []
                _ -> logError "Unknown sync" ["ev" .= ev]

-- | The authorizer assistant handle authorization requests.
authorizerButler :: ButlerState -> ButlerApp -> ProcessIO ButlerAppInstance
authorizerButler _state app = do
    pendingRequests <- newTVarIO []
    let popRequest sessionID = stateTVar pendingRequests \xs ->
            case find (\ev -> ev.client.session.sessionID == sessionID) xs of
                Nothing -> (Nothing, xs)
                Just ev -> (Just ev, filter (\ev' -> ev'.client.session.sessionID /= sessionID) xs)

    let render wid =
            lift (readTVar pendingRequests) >>= \case
                [] -> mempty
                xs -> with div_ [class_ "block flex flex-wrap pb-2"] do
                    div_ "User would like to enter your desktop."
                    forM_ xs \pendingRequest -> do
                        withTrigger "click" wid "desktop-accept-request" ["req" .= pendingRequest.client.session.sessionID] div_ [examplePromptClass] do
                            "authorize "
                            userIcon =<< lift (readTVar pendingRequest.client.session.username)
                        withTrigger "click" wid "desktop-refuse-request" ["req" .= pendingRequest.client.session.sessionID] div_ [examplePromptClass] do
                            "deny "
                            userIcon =<< lift (readTVar pendingRequest.client.session.username)

        handleEvent _ = \case
            ButlerEvent ev ->
                case ev.trigger of
                    "desktop-accept-request" -> case ev.body ^? key "req" . _JSON of
                        Just sessionID ->
                            atomically (popRequest sessionID) >>= \case
                                Nothing -> pure ()
                                Just req -> do
                                    logInfo "Accepting" ["ev" .= ev]
                                    atomically $ putTMVar req.sync.reply (toDyn True)
                        Nothing -> logError "Missing req" ["ev" .= ev]
                    _ -> pure ()
            _ -> pure ()

        handleRequest ev = do
            atomically $ modifyTVar' pendingRequests (ev :)
    pure $ ButlerAppInstance{app, render, handleEvent, handleRequest}

-- | The welcome assistant introduce butler to the user.
data WelcomeState = WNew | WToggled | WDone
    deriving (Generic, FromJSON, ToJSON)

welcomeButler :: ButlerDB -> ButlerState -> ButlerApp -> ProcessIO ButlerAppInstance
welcomeButler db state app = do
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

    let render wid =
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
                        br_ []
                        p_ "You can use the text input below, but I'm not very good at it yet :)"
                WDone -> do
                    "What can I do for you?"
                    renderPromptList state.session wid
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
        handleRequest _ = pure ()

    pure $ ButlerAppInstance{app, handleEvent, handleRequest, render}

renderPromptList :: Session -> AppID -> HtmlT STM ()
renderPromptList session wid = do
    recoverPrompt <-
        lift (readTVar session.recover) >>= \case
            Nothing -> pure "make recovery link"
            Just _ -> pure "show recovery link"
    with div_ [class_ "block w-[128] flex flex-wrap"] do
        traverse_ demoPrompt ["disconnect me", "run welcome", "show tips", recoverPrompt, "help?"]
  where
    demoPrompt (prompt :: Text) =
        withTrigger "click" wid "prompt" ["value" .= prompt] div_ [examplePromptClass] (toHtml prompt)

examplePromptClass :: Attribute
examplePromptClass = class_ "cursor-pointer bg-orange-50 border px-1 rounded-xl ml-1 mb-1"

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
