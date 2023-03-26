-- | This module contains the logic for assistants: personnal programs for helping users.
module Butler.Service.Assistant -- (withAssistantSupervisor, assistantService)
where

import Data.Map.Strict qualified as Map
import Data.UUID.V4 qualified as UUID

import Butler.App
import Butler.AppID
import Butler.Core
import Butler.Core.Pipe
import Butler.Database
import Butler.Display.Client
import Butler.Display.GUI
import Butler.Display.Session
import Butler.Prelude

-- | The supervisor control the assistant by assigning a process per session.
data AssistantSupervisor = AssistantSupervisor
    { processEnv :: ProcessEnv
    , running :: MVar (Map SessionID Assistant)
    , db :: AssistantDB
    }

withAssistantSupervisor :: (AssistantSupervisor -> ProcessIO a) -> ProcessIO a
withAssistantSupervisor cb = withDatabase "assistants" assistantDB \db -> do
    supervisor <- AssistantSupervisor <$> ask <*> newMVar mempty <*> pure (AssistantDB db)
    cb supervisor

-- | Find or create the session assistant.
getAssistant :: AssistantSupervisor -> Session -> ProcessIO Assistant
getAssistant supervisor session = modifyMVar supervisor.running \running ->
    case Map.lookup session.sessionID running of
        Just x -> pure (running, x)
        Nothing -> do
            assistant <- newAssistant supervisor session
            pure (Map.insert session.sessionID assistant running, assistant)

-- | The database stores the raw events to enable the assistant to persist their memory.
newtype AssistantDB = AssistantDB Database

assistantDB :: DatabaseMigration
assistantDB = dbSimpleCreate "events" "session TEXT, uuid TEXT, value BLOB, createdAt DATE, updatedAt DATE"

data AssistantEvent = AssistantEvent
    { uuid :: UUID
    , value :: Value
    , createdAt, updatedAt :: UTCTime
    }

loadEvents :: AssistantDB -> SessionID -> ProcessIO [AssistantEvent]
loadEvents (AssistantDB db) session =
    fmap mkEvent
        <$> dbQuery db "SELECT uuid,value,createdAt,updatedAt FROM events WHERE session = :session" [":session" := session]
  where
    mkEvent (from @UUIDB -> uuid, from @ValueB -> value, createdAt, updatedAt) =
        AssistantEvent{uuid, value, createdAt, updatedAt}

newEvent :: AssistantDB -> SessionID -> Value -> ProcessIO AssistantEvent
newEvent (AssistantDB db) session value = do
    uuid <- liftIO UUID.nextRandom
    now <- liftIO getCurrentTime
    let ev = AssistantEvent uuid value now now
    dbExecute
        db
        "INSERT INTO events (session,uuid,value,createdAt,updatedAt) VALUES (:session, :uuid,:value,:createdAt,:updatedAt)"
        [ ":session" := session
        , ":uuid" := into @UUIDB ev.uuid
        , ":value" := into @ValueB ev.value
        , ":createdAt" := now
        , ":updatedAt" := now
        ]
    pure ev

updateEvent :: AssistantDB -> AssistantEvent -> ProcessIO ()
updateEvent = undefined

deleteEvent :: AssistantDB -> AssistantEvent -> ProcessIO ()
deleteEvent (AssistantDB db) ev =
    dbExecute db "DELETE FROM events WHERE uuid = :uuid" [":uuid" := into @UUIDB ev.uuid]

-- | The service enables access to the assistant.
assistantService :: AssistantSupervisor -> Service
assistantService = Service . defaultApp "assistant" . startAssistantService

startAssistantService :: AssistantSupervisor -> AppContext -> ProcessIO ()
startAssistantService supervisor ctx = do
    let
        sendAssistant ev = do
            assistant <- getAssistant supervisor ev.client.session
            writePipe assistant.pipe ev

    forever do
        atomically (readPipe ctx.pipe) >>= \case
            AppDisplay (UserJoined client) -> sendAssistant (GuiEvent client "join" (toJSON ctx.wid))
            AppDisplay (UserLeft client) -> sendAssistant (GuiEvent client "left" Null)
            AppTrigger ev -> sendAssistant ev
            ev -> logError "Unknown event" ["ev" .= ev]

-- | The assistant actor
data Assistant = Assistant
    { process :: Process
    , pipe :: Pipe GuiEvent
    }

newAssistant :: AssistantSupervisor -> Session -> ProcessIO Assistant
newAssistant supervisor session = do
    pipe <- atomically newPipe
    username <- readTVarIO session.username
    process <-
        asProcess supervisor.processEnv $
            spawnProcess (from $ "assistant-" <> into @Text username) (runAssistant supervisor.db session pipe)
    pure Assistant{process, pipe}

data UserInfo = UserInfo
    { realName :: Maybe Text
    , assistantName :: Maybe Text
    }
    deriving (Generic, FromJSON, ToJSON)

data AssistantStatus = Hidden | Interacting

data AssistantClient = AssistantClient AppID DisplayClient

runAssistant :: AssistantDB -> Session -> Pipe GuiEvent -> ProcessIO ()
runAssistant db session pipe = do
    -- Refresh the assistant memory
    events <- loadEvents db session.sessionID
    let _user :: Maybe (AssistantEvent, UserInfo)
        _user =
            let getUserInfo ev = \case
                    Just x -> Just x
                    Nothing -> case fromJSON ev.value of
                        Success userInfo -> Just (ev, userInfo)
                        _ -> Nothing
             in foldr getUserInfo Nothing events

    -- The state
    status <- newTVarIO Hidden
    (clients :: TVar (Map Endpoint AssistantClient)) <- newTVarIO mempty
    let sendAssistantClients :: (AppID -> HtmlT STM ()) -> ProcessIO ()
        sendAssistantClients html = do
            xs <- readTVarIO clients
            forM_ xs \(AssistantClient wid client) -> do
                atomically $ sendHtml client (html wid)

    -- The UI
    let renderChatUI, hideChatUI, trayUI :: AppID -> HtmlT STM ()
        renderChatUI wid = with div_ [wid_ wid "assistant", class_ "fixed bottom-11 right-2 border p-3 bg-orange-100 w-96 h-96 text-stone-700"] do
            p_ "Welcome to ButlerOS"
            p_ "I am your assistant!"
            br_ []
            input_ [type_ "text", class_ inputClass]

        hideChatUI wid = with div_ [wid_ wid "assistant"] mempty

        trayUI wid = with div_ [wid_ wid "tray"] do
            withTrigger_ "click" wid "toggle-assistant" span_ [class_ "inline-block m-auto ml-1 h-3 w-3 center rounded-full opacity-75 cursor-pointer bg-sky-300"] mempty

        mountUI :: AssistantClient -> ProcessIO ()
        mountUI (AssistantClient wid client) = atomically $ sendHtml client $ with div_ [wid_ wid "tray"] do
            trayUI wid
            lift (readTVar status) >>= \case
                Hidden -> with div_ [wid_ wid "assistant"] mempty
                Interacting -> renderChatUI wid

    -- The event loop
    forever do
        ev <- atomically (readPipe pipe)
        case ev.trigger of
            "join" -> case ev.body ^? _JSON of
                Just wid -> do
                    let client = AssistantClient wid ev.client
                    atomically $ modifyTVar' clients $ Map.insert ev.client.endpoint client
                    mountUI client
                Nothing -> logError "Unknown wid" ["ev" .= ev]
            "left" -> pure ()
            "toggle-assistant" -> do
                newUI <- atomically $ stateTVar status \case
                    Hidden -> (renderChatUI, Interacting)
                    Interacting -> (hideChatUI, Hidden)
                sendAssistantClients newUI
            _ -> logError "Unknown ev" ["ev" .= ev]
