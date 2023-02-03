module Butler.App.LogViewer where

import Butler
import Butler.Logger
import Butler.Prelude

renderLog :: Event SystemEvent -> HtmlT STM ()
renderLog se = toHtml (showT se.createdAt <> " " <> showT se.body)

renderLogs :: OS -> WinID -> HtmlT STM ()
renderLogs os wid = do
    events <- reverse <$> lift (readLogs os.logger)
    with div_ [id_ (withWID wid "w"), class_ ""] do
        with ul_ [id_ (withWID wid "logs-list"), class_ "whitespace-nowrap overflow-x-auto"] do
            traverse_ (li_ . renderLog) events

logViewerApp :: DisplayClients -> App
logViewerApp clients =
    App
        { name = "log-viewer"
        , tags = fromList ["System"]
        , description = "Read event logs"
        , size = Nothing
        , start = \wid pipeDE -> do
            os <- asks os
            do
                chan <- atomically (getLogsChan os.logger)
                forever do
                    ev <- atomically (Left <$> readTChan chan <|> Right <$> readPipe pipeDE)
                    case ev of
                        Right de -> sendHtmlOnConnect (renderLogs os wid) de
                        Left sysEvent ->
                            clientsHtmlT clients do
                                with ul_ [id_ (withWID wid "logs-list"), hxSwapOob_ "afterbegin"] do
                                    li_ $ renderLog sysEvent
        }
