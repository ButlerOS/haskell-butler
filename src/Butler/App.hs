-- | This module contains the logic for graphical app definition.
module Butler.App where

import Data.Map.Strict qualified as Map
import Lucid
import Lucid.Htmx
import Network.WebSockets qualified as WS

import Butler.DisplayClient
import Butler.Dynamic
import Butler.Frame
import Butler.GUI
import Butler.OS
import Butler.Pipe
import Butler.Prelude
import Butler.Session
import Butler.WebSocket (ChannelName)
import Butler.Window

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
    deriving newtype (Ord, Eq, Semigroup, Serialise, IsString, FromJSON, ToJSON)

-- | The type of event an app receive
data AppEvent
    = -- | A display event (e.g. to mount the UI)
      AppDisplay DisplayEvent
    | -- | A trigger event (e.g. onclick)
      AppTrigger GuiEvent
    | -- | A data event (e.g. for raw data)
      AppData DataEvent
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
        pure (wid, AppData (DataEvent client buf rawBuf))

-- | A graphical application definition.
data App = App
    { name :: ProgramName
    -- ^ The application name.
    , tags :: Set AppTag
    -- ^ Its categories.
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
    , devices :: Dynamics
    , apps :: Apps
    }

newAppSharedContext :: Display -> STM AppSharedContext
newAppSharedContext display = AppSharedContext display <$> newDynamics <*> newApps

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
newAppSet = AppSet . Map.fromList . map (\app -> ("app-" <> app.name, app))

launchApp :: AppSet -> ProgramName -> AppSharedContext -> DisplayClients -> WinID -> ProcessIO (Maybe AppInstance)
launchApp (AppSet apps) name shared clients wid = case Map.lookup name apps of
    Just app -> Just <$> startApp app shared clients wid
    Nothing -> pure Nothing

startApp :: App -> AppSharedContext -> DisplayClients -> WinID -> ProcessIO AppInstance
startApp app shared clients wid = do
    -- Start app process
    pipeAE <- atomically newPipe
    let ctx = AppContext clients wid pipeAE shared
    process <- spawnProcess ("app-" <> app.name) do
        app.start ctx

    pure $ AppInstance{app, process, wid, pipeAE}

startApps :: [App] -> Display -> DisplayClients -> ProcessIO AppSharedContext
startApps apps display clients = do
    shared <- atomically (newAppSharedContext display)
    traverse_ (go shared) (zip [0 ..] apps)
    pure shared
  where
    go shared (i, app) = do
        let wid = WinID i
        appInstance <- startApp app shared clients wid
        atomically (registerApp shared.apps appInstance)

appSetHtml :: Monad m => WinID -> AppSet -> HtmlT m ()
appSetHtml wid (AppSet apps) = do
    with ul_ [class_ "list-disc"] do
        forM_ (Map.elems apps) \app -> mkLauncher app.name
  where
    mkLauncher :: ProgramName -> _
    mkLauncher prog =
        with
            li_
            [ wid_ wid "win-swap"
            , encodeVal ["win" .= wid, "prog" .= ("app-" <> prog)]
            , class_ "cursor-pointer"
            , wsSend
            , hxTrigger_ "click"
            ]
            (toHtml prog)
