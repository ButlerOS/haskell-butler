-- | This module contains the logic for graphical app definition.
module Butler.App where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Lucid
import Lucid.Htmx
import Network.WebSockets qualified as WS

import Butler.Core
import Butler.Core.Dynamic
import Butler.Core.File
import Butler.Core.Pipe
import Butler.Display.Client
import Butler.Display.GUI
import Butler.Display.Session
import Butler.Display.WebSocket (ChannelName)
import Butler.Frame
import Butler.Prelude

data Display = Display
    { sessions :: Sessions
    , clients :: TVar (Map SessionID [DisplayClient])
    }

data DisplayEvent
    = UserConnected ChannelName DisplayClient
    | UserDisconnected ChannelName DisplayClient
    deriving (Generic, ToJSON)

instance Show DisplayEvent where
    show = \case
        UserConnected{} -> "UserConnected"
        UserDisconnected{} -> "UserDisconnected"

-- | Application tag.
newtype AppTag = AppTag Text
    deriving (Show, Generic)
    deriving newtype (Ord, Eq, Semigroup, Serialise, IsString, FromJSON, ToJSON, ToHtml)

-- | The type of event an app receive
data AppEvent
    = -- | A display event (e.g. to mount the UI)
      AppDisplay DisplayEvent
    | -- | A trigger event (e.g. onclick)
      AppTrigger GuiEvent
    | -- | A data event (e.g. for raw data)
      AppData DataEvent
    | -- | A file event (e.g. a new file opened)
      AppFile Directory (Maybe File)
    deriving (Generic, ToJSON)

eventFromMessage :: DisplayClient -> WS.DataMessage -> Maybe (WinID, AppEvent)
eventFromMessage client = \case
    WS.Text lbs _ -> do
        htmxEvent <- decodeJSON @HtmxEvent lbs
        (wid, trigger) <- decodeTriggerName htmxEvent.trigger
        pure (wid, AppTrigger (GuiEvent client trigger htmxEvent.body))
    WS.Binary lbs -> do
        let rawBuf = from lbs
        (wid, buf) <- decodeMessage rawBuf
        pure (WinID $ unsafeFrom wid, AppData (DataEvent client buf rawBuf))

-- | A graphical application definition.
data App = App
    { name :: ProgramName
    -- ^ The application name.
    , tags :: Set AppTag
    -- ^ Its categories.
    , title :: Text
    -- ^ Its title.
    , description :: Text
    -- ^ A description.
    , size :: Maybe (Int, Int)
    -- ^ An optional size.
    , xfiles :: [XStaticFile]
    -- ^ Required XStaticFile
    , start :: AppContext -> ProcessIO ()
    -- ^ Start action.
    }

defaultApp :: ProgramName -> (AppContext -> ProcessIO ()) -> App
defaultApp name start =
    App
        { name
        , tags = mempty
        , title = mempty
        , description = mempty
        , size = Nothing
        , xfiles = []
        , start
        }

-- | The application context
data AppContext = AppContext
    { clients :: DisplayClients
    -- ^ the list of all the connected clients. To send update, app should uses `sendsHtml clients ""`
    , wid :: WinID
    -- ^ the instance identifier. The app should mount its UI with `with div_ [wid_ wid] "body"`, and the trigger must container the WinID suffix too.
    , pipe :: Pipe AppEvent
    -- ^ the channel to receive events.
    , shared :: AppSharedContext
    }

data AppSharedContext = AppSharedContext
    { display :: Display
    , processEnv :: ProcessEnv
    , dynamics :: Dynamics
    , apps :: Apps
    }

newAppSharedContext :: Display -> ProcessEnv -> STM AppSharedContext
newAppSharedContext display processEnv = AppSharedContext display processEnv <$> newDynamics <*> newApps

newtype Apps = Apps (TVar (Map WinID AppInstance))

newApps :: STM Apps
newApps = Apps <$> newTVar mempty

unregisterApp :: Apps -> AppInstance -> STM ()
unregisterApp (Apps tv) appInstance = modifyTVar' tv (Map.delete appInstance.wid)

registerApp :: Apps -> AppInstance -> STM ()
registerApp (Apps tv) appInstance = modifyTVar' tv (Map.insert appInstance.wid appInstance)

getApps :: Apps -> STM (Map WinID AppInstance)
getApps (Apps tv) = readTVar tv

data AppInstance = AppInstance
    { app :: App
    , process :: Process
    , wid :: WinID
    , pipeAE :: Pipe AppEvent
    }
    deriving (Generic)

newtype AppSet = AppSet (Map ProgramName App)

-- | A convenient helper to mount the UI when a new user connect.
sendHtmlOnConnect :: HtmlT STM () -> AppEvent -> ProcessIO ()
sendHtmlOnConnect htmlT = \case
    AppDisplay (UserConnected "htmx" client) -> atomically $ sendHtml client htmlT
    _ -> pure ()

newAppSet :: [App] -> AppSet
newAppSet = AppSet . Map.fromList . map (\app -> (app.name, app))

launchApp :: AppSet -> ProgramName -> AppSharedContext -> DisplayClients -> WinID -> ProcessIO (Maybe AppInstance)
launchApp (AppSet apps) (ProgramName name) shared clients wid = case Map.lookup (ProgramName appName) apps of
    Just app -> Just <$> startApp "app-" app shared clients wid
    Nothing -> pure Nothing
  where
    appName = fromMaybe name $ Text.stripPrefix "app-" name

startApp :: Text -> App -> AppSharedContext -> DisplayClients -> WinID -> ProcessIO AppInstance
startApp prefix app shared clients wid = do
    -- Start app process
    pipeAE <- atomically newPipe
    let ctx = AppContext clients wid pipeAE shared
    process <- spawnProcess (from prefix <> app.name) do
        app.start ctx

    pure $ AppInstance{app, process, wid, pipeAE}

startApps :: [App] -> Display -> DisplayClients -> ProcessIO AppSharedContext
startApps apps display clients = do
    processEnv <- ask
    shared <- atomically (newAppSharedContext display processEnv)
    traverse_ (go shared) (zip [0 ..] apps)
    pure shared
  where
    go shared (i, app) = do
        let wid = WinID i
        appInstance <- startApp "app-" app shared clients wid
        atomically (registerApp shared.apps appInstance)

tagIcon :: AppTag -> Maybe Text
tagIcon = \case
    "Communication" -> Just "ri-signal-tower-fill"
    "Development" -> Just "ri-terminal-box-line"
    "Game" -> Just "ri-gamepad-line"
    "Graphic" -> Just "ri-palette-line"
    "Sound" -> Just "ri-volume-up-line"
    "System" -> Just "ri-settings-3-line"
    "Utility" -> Just "ri-tools-line"
    _ -> Nothing

appSetHtml :: Monad m => WinID -> AppSet -> HtmlT m ()
appSetHtml wid (AppSet apps) = do
    ul_ do
        forM_ cats \cat -> do
            with li_ [class_ "mb-2"] do
                forM_ (tagIcon cat) \icon ->
                    with i_ [class_ $ icon <> " text-blue-600 mr-2 text-xl relative top-1"] mempty
                toHtml cat
                with ul_ [class_ "pl-2 border-solid rounded border-l-2 border-slate-500"] do
                    forM_ (appInCat cat) \app -> mkLauncher app.description app.name
  where
    appInCat cat = filter (\app -> Set.member cat app.tags) $ Map.elems apps
    cats = foldMap (.tags) (Map.elems apps)
    mkLauncher :: Text -> ProgramName -> _
    mkLauncher description prog = do
        with
            li_
            [ wid_ wid "win-swap"
            , encodeVal ["prog" .= prog]
            , class_ "cursor-pointer"
            , wsSend
            , hxTrigger_ "click"
            ]
            do
                toHtml prog
                span_ do
                    " ("
                    toHtml description
                    ")"
