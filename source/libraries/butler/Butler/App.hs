-- | This module contains the logic for graphical app definition.
module Butler.App where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Lucid
import Network.WebSockets qualified as WS

import Butler.AppID
import Butler.AppSettings
import Butler.Core
import Butler.Core.Clock
import Butler.Core.Dynamic
import Butler.Core.File
import Butler.Core.Memory
import Butler.Core.Pipe
import Butler.Core.Storage
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

data UserEvent = UserJoined DisplayClient | UserLeft DisplayClient
    deriving (Generic, ToJSON)

-- | The type of event an app receive
data AppEvent
    = -- | A display event (e.g. to mount the UI)
      AppDisplay UserEvent
    | -- | A trigger event (e.g. onclick)
      AppTrigger GuiEvent
    | -- | A data event (e.g. for raw data)
      AppData DataEvent
    | -- | A file event (e.g. a new file opened)
      AppFile Directory (Maybe File)
    | -- | A sync event (e.g. a query that needs a reply)
      AppSync SyncEvent
    | -- | A changed setting event
      AppSettingChanged SettingKey SettingValue SettingValue
    deriving (Generic, ToJSON)

newtype SyncName = SyncName Text
    deriving newtype (IsString, Eq, ToJSON)

data SyncEvent = SyncEvent
    { name :: SyncName
    , reply :: TMVar Dynamic
    }

instance ToJSON SyncEvent where toJSON se = object ["name" .= se.name]

appCall :: Typeable a => AppInstance -> SyncName -> ProcessIO (Maybe a)
appCall appInstance name = do
    mvReply <- newEmptyTMVarIO
    writePipe appInstance.pipe (AppSync (SyncEvent name mvReply))
    res <- atomically =<< waitTransaction 100 (takeTMVar mvReply)
    pure $ case res of
        WaitTimeout -> Nothing
        WaitCompleted dyn -> fromDynamic dyn

eventFromMessage :: DisplayClient -> WS.DataMessage -> Maybe (AppID, AppEvent)
eventFromMessage client = \case
    WS.Text lbs _ -> do
        htmxEvent <- decodeJSON @HtmxEvent lbs
        (wid, TriggerName -> trigger) <- decodeAppID htmxEvent.trigger
        pure (wid, AppTrigger (GuiEvent client trigger htmxEvent.body))
    WS.Binary lbs -> do
        let rawBuf = from lbs
        (wid, buf) <- decodeAppIDMessage rawBuf
        pure (wid, AppData (DataEvent client buf rawBuf))

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
    , settings :: [AppSetting]
    -- ^ The default settings.
    , size :: Maybe (Int, Int)
    -- ^ An optional size.
    , xfiles :: [XStaticFile]
    -- ^ XStaticFile to preload in the page head.
    , extraXfiles :: [XStaticFile]
    -- ^ XStaticFile to be served.
    , acceptFiles :: Maybe ContentType
    , start :: AppContext -> ProcessIO ()
    -- ^ Start action.
    }
    deriving (Generic)

newtype Service = Service App

defaultApp :: ProgramName -> (AppContext -> ProcessIO ()) -> App
defaultApp name start =
    App
        { name
        , tags = mempty
        , title = mempty
        , description = mempty
        , settings = mempty
        , size = Nothing
        , xfiles = []
        , extraXfiles = []
        , acceptFiles = Nothing
        , start
        }

-- | The application context
data AppContext = AppContext
    { wid :: AppID
    -- ^ the instance identifier. The app should mount its UI with `with div_ [wid_ wid] "body"`, and the trigger must container the AppID suffix too.
    , pipe :: Pipe AppEvent
    -- ^ the channel to receive events.
    , argv :: Maybe Value
    -- ^ the application arguments
    , shared :: AppSharedContext
    }

data AppSharedContext = AppSharedContext
    { display :: Display
    , processEnv :: ProcessEnv
    , appSet :: AppSet
    , clients :: DisplayClients
    -- ^ the list of all the connected clients. To send update, app should uses `sendsHtml clients ""`
    , dynamics :: Dynamics
    , appIDCounter :: AppIDCounter
    , apps :: Apps
    , extraHandlers :: TVar (Map ChannelName (DisplayEvent -> ProcessIO ()))
    }

newAppSharedContext :: MonadIO m => Display -> ProcessEnv -> AppSet -> m AppSharedContext
newAppSharedContext display processEnv appSet =
    AppSharedContext display processEnv appSet
        <$> atomically newDisplayClients
        <*> newDynamics
        <*> atomically newAppIDCounter
        <*> atomically newApps
        <*> newTVarIO mempty

newtype Apps = Apps (TVar (Map AppID AppInstance))

newApps :: STM Apps
newApps = Apps <$> newTVar mempty

unregisterApp :: Apps -> AppInstance -> STM ()
unregisterApp (Apps tv) appInstance = modifyTVar' tv (Map.delete appInstance.wid)

registerApp :: Apps -> AppInstance -> STM ()
registerApp (Apps tv) appInstance = modifyTVar' tv (Map.insert appInstance.wid appInstance)

getApps :: Apps -> STM (Map AppID AppInstance)
getApps (Apps tv) = readTVar tv

data AppInstance = AppInstance
    { app :: App
    , process :: Process
    , wid :: AppID
    , pipe :: Pipe AppEvent
    , argv :: Maybe Value
    }
    deriving (Generic)

newtype AppSet = AppSet {appSetMap :: Map ProgramName App}

lookupAppSet :: ProgramName -> AppSet -> Maybe App
lookupAppSet program (AppSet m) = Map.lookup program m

appSetApps :: AppSet -> [App]
appSetApps (AppSet m) = Map.elems m

getAppSettings :: AppSharedContext -> ProgramName -> ProcessIO (Map SettingKey SettingValue)
getAppSettings shared program = do
    -- read saved settings
    mvSettings <- getSettings shared.dynamics
    setting <- atomically (lookupSetting mvSettings program)
    -- add default settings
    let setDefault :: AppSetting -> (Map SettingKey SettingValue -> Map SettingKey SettingValue)
        setDefault appSetting = case Map.lookup appSetting.name setting of
            Nothing -> Map.insert appSetting.name appSetting.value
            Just _ -> id
    pure $ composeFunctions (setDefault <$> appSettings) setting
  where
    appSettings = case Map.lookup program shared.appSet.appSetMap of
        Just app -> app.settings
        Nothing -> []

-- | A convenient helper to mount the UI when a new user connect.
sendHtmlOnConnect :: HtmlT STM () -> AppEvent -> ProcessIO ()
sendHtmlOnConnect htmlT = \case
    AppDisplay (UserJoined client) -> atomically $ sendHtml client htmlT
    _ -> pure ()

newAppSet :: [App] -> AppSet
newAppSet = AppSet . Map.fromList . map (\app -> (app.name, app))

launchApp :: AppSet -> ProgramName -> Maybe Value -> AppSharedContext -> AppID -> ProcessIO (Maybe AppInstance)
launchApp (AppSet apps) (ProgramName name) argv shared wid = case Map.lookup (ProgramName appName) apps of
    Just app -> Just <$> startApp "app-" app argv shared wid
    Nothing -> pure Nothing
  where
    appName = fromMaybe name $ Text.stripPrefix "app-" name

startApp :: Text -> App -> Maybe Value -> AppSharedContext -> AppID -> ProcessIO AppInstance
startApp prefix app argv shared wid = do
    -- Start app process
    pipe <- atomically newPipe
    let ctx = AppContext wid pipe argv shared
    process <- spawnProcess (from prefix <> app.name) do
        app.start ctx
    let appInstance = AppInstance{app, process, wid, pipe, argv}
    atomically (registerApp shared.apps appInstance)
    pure appInstance

-- | Start the application that is in charge of starting the other apps.
startShellApp :: AppSet -> Text -> App -> Display -> ProcessIO (AppSharedContext, AppInstance)
startShellApp appSet prefix app display = do
    let wid = shellAppID
        argv = Nothing
    pipe <- atomically newPipe
    mvShared <- newEmptyMVar
    process <- spawnProcess (from prefix <> app.name) do
        processEnv <- ask
        shared <- newAppSharedContext display processEnv appSet
        putMVar mvShared shared
        withSettings shared.dynamics do
            app.start (AppContext wid pipe argv shared)
    shared <- takeMVar mvShared
    let appInstance = AppInstance{app, process, wid, pipe, argv}
    atomically (registerApp shared.apps appInstance)
    pure (shared, appInstance)

startApps :: [App] -> Display -> ProcessIO AppSharedContext
startApps apps display = do
    processEnv <- ask
    shared <- newAppSharedContext display processEnv (newAppSet apps)
    traverse_ (go shared) apps
    pure shared
  where
    go shared app = do
        wid <- atomically (nextAppID shared.appIDCounter =<< getApps shared.apps)
        startApp "app-" app Nothing shared wid

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

-- This needs to be kept in sync with the Butler.Frame.butlerHelpersScript javascript implementation 'withWID'
withWID :: AppID -> Text -> Text
withWID winID n = n <> "-" <> showT winID

dropWID :: Text -> Text
dropWID = Text.dropWhileEnd (== '-') . Text.dropWhileEnd isDigit

wid_ :: AppID -> Text -> _
wid_ wid n = id_ (withWID wid n)

withTrigger_ :: With a => Text -> AppID -> TriggerName -> a -> [Attribute] -> a
withTrigger_ hxTrigger wid (TriggerName trigger) elt attrs =
    with elt (wid_ wid trigger : wsSend : hxTrigger_ hxTrigger : attrs)

withTrigger :: With a => Text -> AppID -> TriggerName -> [Pair] -> a -> [Attribute] -> a
withTrigger hxTrigger wid trigger vals elt attrs = withTrigger_ hxTrigger wid trigger elt (encodeVal vals : attrs)

-- Make the Html element to Trigger an HTMX event on Natural Element Event (https://htmx.org/docs/#triggers)
withEvent :: Monad m => AppID -> TriggerName -> [Pair] -> HtmlT m () -> HtmlT m ()
withEvent appId (TriggerName trigger) vals elm =
    with
        elm
        (wid_ appId trigger : wsSend : encodeVal vals : mempty)

-- Make the Html element to Trigger an HTMX event on Custom Event
withCustomEvent :: Monad m => Text -> AppID -> TriggerName -> [Pair] -> HtmlT m () -> HtmlT m ()
withCustomEvent eventType appId (TriggerName trigger) vals elm =
    with
        elm
        (wid_ appId trigger : wsSend : encodeVal vals : [hxTrigger_ eventType])

-- Make the Html element to Trigger an HTMX event on Click event
withClickEvent :: Monad m => AppID -> TriggerName -> [Pair] -> HtmlT m () -> HtmlT m ()
withClickEvent = withCustomEvent "click"

butlerCheckbox :: AppID -> Text -> [Pair] -> Bool -> Maybe Text -> [Attribute]
butlerCheckbox wid name attrs value mConfirm
    | value = checked_ : attributes
    | otherwise = attributes
  where
    baseAction = sendTriggerScript wid name attrs
    action = case mConfirm of
        Just txt -> "if (window.confirm(\"" <> txt <> "\")) {" <> baseAction <> ";}"
        Nothing -> baseAction
    attributes = [type_ "checkbox", onclick_ (action <> "; return false")]

sendTriggerScriptConfirm :: AppID -> Text -> [Pair] -> Maybe Text -> Text
sendTriggerScriptConfirm wid name attrs mConfirm = case mConfirm of
    Nothing -> script
    Just txt -> "if (window.confirm(\"" <> txt <> "\")) {" <> script <> ";}"
  where
    script = sendTriggerScript wid name attrs

sendTriggerScript :: AppID -> Text -> [Pair] -> Text
sendTriggerScript wid name attrs =
    "sendTrigger(" <> showT wid <> ", \"" <> name <> "\", " <> decodeUtf8 (from obj) <> ")"
  where
    obj = encodeJSON (object attrs)

onChangeTrigger :: AppID -> Text -> _
onChangeTrigger wid name =
    onchange_ $
        "sendTrigger(" <> showT wid <> ", \"" <> name <> "\", {value: this.value})"

startAppScript :: App -> [Pair] -> Text
startAppScript app args = sendTriggerScript shellAppID "start-app" (["name" .= app.name] <> args)

closeApp :: AppSharedContext -> DisplayClient -> AppID -> ProcessIO ()
closeApp shared client wid = do
    atomically (Map.lookup shellAppID <$> getApps shared.apps) >>= \case
        Just shell -> do
            let msg = encodeJSON $ object ["ev" .= ("close" :: Text), "w" .= wid]
                rawMsg = encodeMessage 0 msg
            writePipe shell.pipe (AppData $ DataEvent client msg rawMsg)
        Nothing -> logError "Can't find shell" []

appSetHtml :: Monad m => AppID -> AppSet -> HtmlT m ()
appSetHtml wid (AppSet apps) = do
    ul_ do
        forM_ cats \cat -> do
            with li_ [class_ "mb-2"] do
                forM_ (tagIcon cat) \icon ->
                    with i_ [class_ $ icon <> " text-blue-600 mr-2 text-xl relative top-1"] mempty
                toHtml cat
                with ul_ [class_ "pl-2 border-solid rounded border-l-2 border-slate-500"] do
                    forM_ (appInCat cat) mkLauncher
  where
    appInCat cat = filter (\app -> Set.member cat app.tags) $ Map.elems apps
    cats = foldMap (.tags) (Map.elems apps)
    mkLauncher app = do
        with
            li_
            [ onclick_ (startAppScript app ["wid" .= wid])
            , class_ "cursor-pointer"
            ]
            do
                toHtml app.name
                span_ do
                    " ("
                    toHtml app.description
                    ")"

newAppMemory :: Serialise a => AppID -> StorageAddress -> a -> ProcessIO (a, MemoryVar a)
newAppMemory wid addr value = do
    baseDir <- getPath (from wid)
    liftIO $ createDirectoryIfMissing True (from $ decodeUtf8 baseDir)
    newProcessMemory (mconcat [from wid, "/", addr]) (pure value)

delAppMemory :: AppID -> ProcessIO ()
delAppMemory wid = do
    os <- asks os
    removeStorage os.storage (from wid)

-- | A minimal app to serve html
htmlApp :: [XStaticFile] -> Html () -> App
htmlApp xfiles mountUI = (defaultApp "html" serveHtml){xfiles}
  where
    serveHtml :: AppContext -> ProcessIO ()
    serveHtml ctx = do
        forever do
            atomically (readPipe ctx.pipe) >>= \case
                AppDisplay (UserJoined client) -> atomically $ sendHtml client $ hoistHtml do
                    with div_ [wid_ ctx.wid "w"] do
                        mountUI
                _ -> pure ()
