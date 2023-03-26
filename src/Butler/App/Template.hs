{- | A template ready to be used.

Copy the file and replace 'template' with the name of your app.
-}
module Butler.App.Template (templateApp) where

import Butler

templateApp :: App
templateApp =
    (defaultApp "template" startTemplate)
        { tags = fromList ["Utility"]
        , description = "A template app"
        }

startTemplate :: AppContext -> ProcessIO ()
startTemplate ctx = do
    -- Setup state
    logInfo "Template started!" []

    -- UI
    let mountUI :: HtmlT STM ()
        mountUI = with div_ [wid_ ctx.wid "w", class_ "flex flex-col"] do
            div_ "Template"
            withTrigger_ "click" ctx.wid "trigger-name" button_ [] "Click me!"

    -- Handle events
    forever do
        atomically (readPipe ctx.pipe) >>= \case
            AppDisplay (UserJoined client) -> atomically $ sendHtml client mountUI
            AppTrigger ev -> case ev.trigger of
                "trigger-name" -> logInfo "Clicked!" ["ev" .= ev]
                _ -> logError "Unknown trigger" ["ev" .= ev]
            ev -> logError "Unknown ev" ["ev" .= ev]
