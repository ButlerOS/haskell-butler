{- | A template ready to be used.

Copy the file and replace 'template' with the name of your app.
-}
module Butler.App.Template (templateApp) where

import Butler
import Butler.AppSettings

templateApp :: App
templateApp =
    (defaultApp "template" startTemplate)
        { tags = fromList ["Utility"]
        , description = "A template app"
        , settings =
            [ AppSetting "test-url" "" SettingURL
            , AppSetting "test-token" "" SettingToken
            ]
        }

data TemplateState = Clicked | Input Text

startTemplate :: AppContext -> ProcessIO ()
startTemplate ctx = do
    -- Setup state
    logInfo "Template started!" []
    state <- newTVarIO Nothing
    let setState = atomically . writeTVar state . Just

    -- UI
    let mountUI :: HtmlT STM ()
        mountUI = with div_ [wid_ ctx.wid "w", class_ "flex flex-col"] do
            div_ "Template"
            withTrigger_ "click" ctx.wid "trigger-name" button_ [] "Click me!"
            withTrigger_ "" ctx.wid "input-name" (input_ []) [type_ "text", placeholder_ "Type and enter...", name_ "value", class_ inputClass]
            div_ do
                lift (readTVar state) >>= \case
                    Just Clicked -> "Clicked!"
                    Just (Input txt) -> "Input: " <> toHtml txt
                    Nothing -> pure ()

    -- Handle events
    forever do
        atomically (readPipe ctx.pipe) >>= \case
            AppDisplay (UserJoined client) -> atomically $ sendHtml client mountUI
            AppTrigger ev -> do
                case ev.trigger of
                    "trigger-name" -> setState Clicked
                    "input-name" -> do
                        case ev.body ^? key "value" . _String of
                            Just txt -> setState (Input txt)
                            Nothing -> logError "Missing value" ["ev" .= ev]
                    _ -> logError "Unknown trigger" ["ev" .= ev]
                sendsHtml ctx.shared.clients mountUI
            ev -> logError "Unknown ev" ["ev" .= ev]
