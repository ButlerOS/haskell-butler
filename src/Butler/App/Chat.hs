module Butler.App.Chat (
    ChatServer,
    UserMessage (..),
    newChatServer,
    newChatReader,
    updateChat,
    addUserMessage,
    renderChat,
    chatApp,
) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

import Butler
import Butler.History
import Butler.Prelude
import Butler.Session
import Butler.User

data ChatServer = ChatServer
    { allClients :: TVar (Map SessionID [DisplayClient])
    , history :: History UserMessage
    , events :: BroadcastChan UserEvent
    , users :: TVar (Set UserName)
    }

data UserMessage = MkMessage
    { author :: UserName
    , body :: Text
    }
    deriving (Generic, ToJSON)

data UserEvent = UserJoined UserName | UserLeft UserName | UserChat UserMessage
    deriving (Generic, ToJSON)

newChatServer :: TVar (Map SessionID [DisplayClient]) -> STM ChatServer
newChatServer allClients = demoChat =<< (ChatServer allClients <$> newHistory 42 <*> newBroadcastChan <*> newTVar mempty)

newChatReader :: ChatServer -> STM (TChan UserEvent)
newChatReader srv = newReaderChan srv.events

demoChat :: ChatServer -> STM ChatServer
demoChat srv = do
    addHistory srv.history (MkMessage "alice" "hi!")
    addHistory srv.history (MkMessage "bob" "The quick brown fox jumps over the lazy dog")
    pure srv

handleDisplayEvent :: ChatServer -> DisplayEvent -> STM ()
handleDisplayEvent srv = \case
    UserConnected _ client -> do
        let user = client.session.username
        added <- stateTVar srv.users $ \users ->
            if Set.member user users
                then (False, users)
                else (True, Set.insert user users)
        when added do
            let ev = UserJoined user
            broadcast srv.events ev
    UserDisconnected _ client -> do
        let user = client.session.username
        removed <- null . fromMaybe [] . Map.lookup client.session.sessionID <$> readTVar srv.allClients
        when removed do
            let ev = UserLeft user
            modifyTVar' srv.users (Set.delete user)
            broadcast srv.events ev

addUserMessage :: ChatServer -> UserMessage -> STM ()
addUserMessage srv um = do
    broadcast srv.events (UserChat um)
    addHistory srv.history um

updateChat :: WinID -> DisplayClient -> UserEvent -> HtmlT STM ()
updateChat wid client = \case
    UserChat um -> appendUserMessage wid client um
    UserJoined user -> appendUser wid client user
    UserLeft user -> with div_ [id_ (withWID wid ("chat-" <> from user)), hxSwapOob_ "delete"] mempty

appendUserMessage :: WinID -> DisplayClient -> UserMessage -> HtmlT STM ()
appendUserMessage wid _client um = do
    with div_ [id_ (withWID wid "chat-history"), hxSwapOob_ "afterbegin"] do
        renderUM um
    inputHtml

renderUM :: UserMessage -> HtmlT STM ()
renderUM um = do
    div_ do
        with span_ [class_ "font-bold", userColorStyle um.author] (toHtml um.author)
        ": "
        span_ (toHtml um.body)

appendUser :: WinID -> DisplayClient -> UserName -> HtmlT STM ()
appendUser wid client user = do
    with div_ [id_ (withWID wid "chat-list"), hxSwapOob_ "beforeend"] do
        renderUser wid client user

renderUser :: WinID -> DisplayClient -> UserName -> HtmlT STM ()
renderUser wid client user = do
    with li_ (highlightSelf [id_ (withWID wid ("chat-" <> from user))]) $ userIcon user
  where
    highlightSelf
        | client.session.username == user = (class_ "font-bold border border-black rounded-full" :)
        | otherwise = id

inputHtml :: HtmlT STM ()
inputHtml =
    with (input_ mempty) [class_ "w-full", id_ "chat-input", name_ "message", type_ "text", placeholder_ "Chat message"]

renderChat :: WinID -> ChatServer -> DisplayClient -> HtmlT STM ()
renderChat wid srv client = do
    with div_ [id_ (withWID wid "w"), class_ "border"] do
        with div_ [class_ "flex"] do
            with div_ [id_ (withWID wid "chat-history"), class_ $ heightLimit <> "border overflow-auto grow flex flex-col-reverse"] do
                history <- lift (recentHistory srv.history)
                traverse_ renderUM history
            with ul_ [id_ (withWID wid "chat-list"), class_ "flex w-7 flex-col overflow-y-auto"] do
                users <- lift (readTVar srv.users)
                traverse_ (renderUser wid client) users

        with form_ [wsSend, hxTrigger_ "submit", id_ $ withWID wid "chat-message"] do
            inputHtml
  where
    heightLimit = case wid of
        WinID 0 -> "max-h-44 "
        _ -> "max-h-72 "

chatApp :: ChatServer -> WithGuiEvents -> App
chatApp srv withGuiEvent =
    App
        { name = "chat"
        , tags = fromList ["Communication"]
        , description = "Local chat room"
        , size = Nothing
        , start = withGuiEvent (startChatApp srv)
        }

startChatApp :: ChatServer -> GuiEvents -> AppStart
startChatApp srv guiEvents wid pipeDE = do
    chatChan <- atomically (newChatReader srv)
    spawnThread_ $ forever do
        ev <- atomically (readTChan chatChan)
        logInfo "Gto chat" ["ev" .= ev]
        clientsDraw guiEvents.clients (\client -> pure $ updateChat wid client ev)

    let handleGuiEvent ev =
            case ev.body ^? key "message" . _String of
                Just msg -> atomically $ addUserMessage srv (MkMessage ev.client.session.username msg)
                Nothing -> logError "bad chat ev" ["ev" .= ev]

    forever do
        ev <- atomically $ readPipe2 pipeDE guiEvents.pipe
        case ev of
            Left de -> do
                case de of
                    UserConnected "htmx" client -> do
                        atomically $ sendHtml client (renderChat wid srv client)
                        atomically (handleDisplayEvent srv de)
                    UserDisconnected _ _ ->
                        atomically (handleDisplayEvent srv de)
                    _ -> pure ()
            Right ge -> handleGuiEvent ge
