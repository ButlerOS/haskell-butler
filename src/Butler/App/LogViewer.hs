module Butler.App.LogViewer where

import Butler.Prelude

import Butler.Desktop
import Butler.GUI
import Butler.Logger
import Butler.Window

renderLog :: Event SystemEvent -> HtmlT STM ()
renderLog se = toHtml (showT se.createdAt <> " " <> showT se.body)

renderLogs :: OS -> WinID -> HtmlT STM ()
renderLogs os wid = do
    events <- reverse <$> lift (readLogs os.logger)
    with div_ [id_ (withWID wid "w"), class_ ""] do
        with ul_ [id_ (withWID wid "logs-list"), class_ "whitespace-nowrap overflow-x-auto"] do
            traverse_ (li_ . renderLog) events

logViewerApp :: Desktop -> WinID -> ProcessIO GuiApp
logViewerApp desktop wid = do
    os <- asks os
    newGuiApp "log-viewer" Nothing (const . pure $ renderLogs os wid) (scopeTriggers wid ["ps-kill", "ps-toggle"]) \app -> do
        spawnThread_ $ forever do
            ev <- atomically $ readPipe app.events
            logError "unknown event" ["ev" .= ev]
        chan <- atomically (getLogsChan os.logger)
        forever do
            sysEvent <- atomically $ readTChan chan
            broadcastMessageT desktop do
                with ul_ [id_ (withWID wid "logs-list"), hxSwapOob_ "afterbegin"] do
                    li_ $ renderLog sysEvent
