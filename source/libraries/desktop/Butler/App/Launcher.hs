module Butler.App.Launcher (launcherApp) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text

import Butler
import Butler.App

launcherApp :: App
launcherApp =
    (defaultApp "launcher" startLauncherApp)
        { description = "Start apps"
        , tags = fromList ["Utility"]
        }

startLauncherApp :: AppContext -> ProcessIO ()
startLauncherApp ctx = do
    vFilter <- newTVarIO (mempty :: Text)
    let filterAppSet txt
            | txt == mempty = ctx.shared.appSet
            | otherwise = AppSet $ Map.filter appMatch (coerce ctx.shared.appSet)
          where
            appMatch app =
                Text.isInfixOf txt (coerce app.name)
                    || coerce (Text.toTitle txt) `Set.member` app.tags
        drawAppList filterText = do
            with div_ [wid_ ctx.wid "app-list"] do
                appSetHtml ctx.wid (filterAppSet filterText)
        mountUI = do
            with div_ [wid_ ctx.wid "w"] do
                filterText <- lift (readTVar vFilter)
                with form_ [wid_ ctx.wid "launch", wsSend, hxTrigger_ "submit"] do
                    input_
                        [ class_ "form-control"
                        , size_ "42"
                        , type_ "text"
                        , name_ "query"
                        , wid_ ctx.wid "filter"
                        , value_ filterText
                        , placeholder_ "Filter list"
                        , wsSend
                        , autofocus_
                        , hxTrigger_ "keyup changed delay:60ms, search"
                        ]
                drawAppList filterText
    forever do
        atomically (readPipe ctx.pipe) >>= \case
            ae@AppDisplay{} -> sendHtmlOnConnect mountUI ae
            AppTrigger ev -> case ev.trigger of
                "filter" -> case ev.body ^? key "query" . _JSON of
                    Just filterText -> do
                        atomically $ writeTVar vFilter filterText
                        -- Render the full UI for everyone
                        sendsHtmlButSelf ev.client ctx.shared.clients mountUI
                        -- Render the list for the active user to keep the input intact
                        atomically $ sendHtml ev.client (drawAppList filterText)
                    Nothing -> logError "Unknown filter" ["ev" .= ev]
                "launch" -> case ev.body ^? key "query" . _JSON of
                    Just filterText -> case Map.elems (coerce $ filterAppSet filterText) of
                        [app :: App] -> do
                            logInfo "Launching" ["app" .= app.name]
                            let swapWin = with div_ [wid_ ctx.wid "w"] do
                                    script_ $ startAppScript app ["wid" .= ctx.wid]
                            atomically $ sendHtml ev.client swapWin
                        _ -> do
                            atomically $ writeTVar vFilter mempty
                            sendsHtml ctx.shared.clients mountUI
                    Nothing -> logError "Unknown filter" ["ev" .= ev]
                _ -> logError "Unknown trigger" ["ev" .= ev]
            ev -> logError "Unknown ev" ["ev" .= ev]
