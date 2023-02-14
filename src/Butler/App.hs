module Butler.App where

import Data.Map.Strict qualified as Map
import Network.WebSockets qualified as WS

import Butler.Display
import Butler.Frame
import Butler.GUI
import Butler.Prelude
import Butler.Window

newtype AppTag = AppTag Text
    deriving (Show, Generic)
    deriving newtype (Ord, Eq, Semigroup, Serialise, IsString, FromJSON, ToJSON)

data AppEvent
    = AppDisplay DisplayEvent
    | AppTrigger GuiEvent
    | AppData DataEvent
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

type AppStart = DisplayClients -> WinID -> Pipe AppEvent -> ProcessIO ()

data App = App
    { name :: ProgramName
    , tags :: Set AppTag
    , description :: Text
    , size :: Maybe (Int, Int)
    , start :: AppStart
    }

data AppInstance = AppInstance
    { app :: App
    , process :: Process
    , wid :: WinID
    , pipeAE :: Pipe AppEvent
    }
    deriving (Generic)

newtype AppSet = AppSet (Map ProgramName App)

sendHtmlOnConnect :: HtmlT STM () -> AppEvent -> ProcessIO ()
sendHtmlOnConnect htmlT = \case
    AppDisplay (UserConnected "htmx" client) -> atomically $ sendHtml client htmlT
    _ -> pure ()

newAppSet :: [App] -> AppSet
newAppSet = AppSet . Map.fromList . map (\app -> ("app-" <> app.name, app))

launchApp :: AppSet -> ProgramName -> DisplayClients -> WinID -> ProcessIO (Maybe AppInstance)
launchApp (AppSet apps) name clients wid = case Map.lookup name apps of
    Just app -> Just <$> startApp app clients wid
    Nothing -> pure Nothing

startApp :: App -> DisplayClients -> WinID -> ProcessIO AppInstance
startApp app clients wid = do
    -- Start app process
    pipeAE <- atomically newPipe
    process <- spawnProcess ("app-" <> app.name) do
        app.start clients wid pipeAE

    pure $ AppInstance{app, process, wid, pipeAE}

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
