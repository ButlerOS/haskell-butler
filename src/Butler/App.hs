module Butler.App where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

import Butler.GUI
import Butler.Prelude
import Butler.Window

newtype AppTag = AppTag Text
    deriving (Show, Generic)
    deriving newtype (Ord, Eq, Semigroup, Serialise, IsString, FromJSON, ToJSON)

data App = App
    { name :: ProgramName
    , tags :: Set AppTag
    , description :: Text
    , size :: Maybe (Int, Int)
    , triggers :: [TriggerName]
    , start :: DisplayClients -> WinID -> ProcessIO AppInstance
    }

data AppInstance = AppInstance
    { draw :: DrawHtml
    , drawTray :: DrawHtml
    , run :: Pipe GuiEvent -> ProcessIO Void
    }
    deriving (Generic)

newAppInstance :: Monad m => DrawHtml -> (Pipe GuiEvent -> ProcessIO Void) -> m AppInstance
newAppInstance draw run = pure AppInstance{run, draw, drawTray}
  where
    drawTray = emptyDraw

newtype AppSet = AppSet (Map ProgramName App)

newAppSet :: [App] -> AppSet
newAppSet = AppSet . Map.fromList . map (\app -> ("app-" <> app.name, app))

launchApp :: AppSet -> ProgramName -> DisplayClients -> WinID -> ProcessIO (Maybe GuiApp)
launchApp (AppSet apps) name ctx wid = case Map.lookup name apps of
    Just app -> Just <$> startApp app ctx wid
    Nothing -> pure Nothing

startApp :: App -> DisplayClients -> WinID -> ProcessIO GuiApp
startApp app ctx wid = do
    events <- atomically newPipe

    -- Start app process
    mvAppInstance <- newEmptyTMVarIO
    process <- spawnProcess ("app-" <> app.name) do
        appInstance <- app.start ctx wid
        atomically $ putTMVar mvAppInstance appInstance
        void $ appInstance.run events

    appInstance <- atomically (readTMVar mvAppInstance)
    let triggers
            | wid == WinID 0 = fromList app.triggers
            | otherwise = Set.fromList (scopeTriggers wid app.triggers)
        draw client = do
            body <- appInstance.draw client
            pure $ with div_ [id_ (withWID wid "w")] body
        drawMenu = emptyDraw
        drawTray = appInstance.drawTray
    size <- case app.size of
        Nothing -> pure Nothing
        Just sz -> Just <$> newTVarIO sz

    pure $ GuiApp{process, triggers, draw, drawMenu, drawTray, events, size}

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
