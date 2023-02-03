module Butler.App where

import Data.Map.Strict qualified as Map

import Butler.Display
import Butler.Frame
import Butler.GUI
import Butler.Prelude

data GuiEvents = GuiEvents
    { clients :: DisplayClients
    , pipe :: Pipe GuiEvent
    }
type WithGuiEvents = (GuiEvents -> AppStart) -> AppStart
type WithNamedGuiEvents = [TriggerName] -> WithGuiEvents

data DataEvents = DataEvents
    { clients :: DisplayClients
    , chan :: ChannelID
    , pipe :: Pipe DataEvent
    }
type WithDataEvents = (DataEvents -> AppStart) -> AppStart

standaloneGuiEvents :: DisplayClients -> Pipe GuiEvent -> WithGuiEvents
standaloneGuiEvents clients pipe cb = cb (GuiEvents{clients, pipe})

newtype AppTag = AppTag Text
    deriving (Show, Generic)
    deriving newtype (Ord, Eq, Semigroup, Serialise, IsString, FromJSON, ToJSON)

type AppStart = WinID -> Pipe DisplayEvent -> ProcessIO ()

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
    , pipeDisplayEvents :: Pipe DisplayEvent
    }
    deriving (Generic)

newtype AppSet = AppSet (Map ProgramName App)

pureHtmlApp :: (WinID -> HtmlT STM ()) -> AppStart
pureHtmlApp mkHtml wid pipeDE = forever do
    atomically (readPipe pipeDE) >>= sendHtmlOnConnect (mkHtml wid)

sendHtmlOnConnect :: HtmlT STM () -> DisplayEvent -> ProcessIO ()
sendHtmlOnConnect htmlT = \case
    UserConnected "htmx" client -> atomically $ sendHtml client htmlT
    _ -> pure ()

newAppSet :: [App] -> AppSet
newAppSet = AppSet . Map.fromList . map (\app -> ("app-" <> app.name, app))

launchApp :: AppSet -> ProgramName -> WinID -> ProcessIO (Maybe AppInstance)
launchApp (AppSet apps) name wid = case Map.lookup name apps of
    Just app -> Just <$> startApp app wid
    Nothing -> pure Nothing

startApp :: App -> WinID -> ProcessIO AppInstance
startApp app wid = do
    -- Start app process
    pipeDisplayEvents <- atomically newPipe
    process <- spawnProcess ("app-" <> app.name) do
        app.start wid pipeDisplayEvents

    pure $ AppInstance{app, process, wid, pipeDisplayEvents}

appSetHtml :: Monad m => WinID -> AppSet -> HtmlT m ()
appSetHtml wid (AppSet apps) = do
    with ul_ [class_ "list-disc"] do
        forM_ (Map.elems apps) \app -> mkLauncher app.name
  where
    mkLauncher :: ProgramName -> _
    mkLauncher prog =
        with
            li_
            [ id_ "win-swap"
            , encodeVal ["win" .= wid, "prog" .= ("app-" <> prog)]
            , class_ "cursor-pointer"
            , wsSend
            , hxTrigger_ "click"
            ]
            (toHtml prog)
