-- | The setting app contains the logic to change the settings.
module Butler.App.Settings where

import Data.Map.Strict qualified as Map

import Butler
import Butler.App
import Butler.AppSettings
import Butler.Core

settingsApp :: App
settingsApp =
    (defaultApp "settings" startSettings)
        { tags = fromList ["Utility"]
        , description = "A settings app"
        }

startSettings :: AppContext -> ProcessIO ()
startSettings ctx = do
    -- Setup state
    mvSettings <- getSettings ctx.shared.dynamics

    -- UI
    let selectUI :: ProgramName -> AppSetting -> HtmlT STM ()
        selectUI program appSetting = with form_ [wsSend, hxTrigger_ "change", wid_ ctx.wid "set", onsubmit_ "return false"] do
            with (input_ mempty) [type_ "hidden", name_ "setting", value_ (from appSetting.name)]
            with (input_ mempty) [type_ "hidden", name_ "program", value_ (from program)]
            case appSetting.schema of
                SettingChoice xs -> with select_ [name_ "value"] do
                    option_ "default"
                    traverse_ (option_ . toHtml) xs
                SettingToggle -> with (input_ mempty) [name_ "value", type_ "checkbox", onclick_ "alert('Not Implemented!'); return false"]
                SettingURL -> with (input_ mempty) [name_ "value", type_ "text"]
                SettingName -> with (input_ mempty) [name_ "value", type_ "text"]
                SettingToken -> with (input_ mempty) [name_ "value", type_ "password"]
        -- let fakeValue = case appSetting.value of
        --         "" -> ""
        --         _ -> "xxxx"
        --  in with (input_ mempty) [name_ "value", type_ "password", value_ fakeValue]

        mountUI :: HtmlT STM ()
        mountUI = with div_ [wid_ ctx.wid "w", class_ "flex flex-col"] do
            settings <- lift (readMemoryVar mvSettings)
            with table_ [class_ "border w-full"] do
                thead_ $ tr_ do
                    th_ "Key"
                    th_ "Value"
                    th_ "Set"
                tbody_ do
                    forM_ (appSetApps ctx.shared.appSet) \app -> case app.settings of
                        [] -> pure ()
                        appSettings -> do
                            tr_ do
                                with td_ [colspan_ "3", class_ "font-bold"] $ "> " >> toHtml app.name
                            let savedSettings :: Map SettingKey SettingValue
                                savedSettings = getSetting app.name settings
                            forM_ appSettings \appSetting -> do
                                let mUserSetting = Map.lookup appSetting.name savedSettings
                                    settingValue = fromMaybe appSetting.value mUserSetting
                                tr_ do
                                    td_ $ toHtml appSetting.name
                                    td_ do
                                        case mUserSetting of
                                            Just{} -> withTrigger "click" ctx.wid "clear" ["setting" .= appSetting.name, "program" .= app.name] i_ [class_ "ri-delete-bin-2-fill text-red-500 mx-2 cursor-pointer", title_ "Clear the setting"] mempty
                                            Nothing -> pure ()
                                        case appSetting.schema of
                                            SettingToken -> case mUserSetting of
                                                Just{} -> "******"
                                                Nothing -> ""
                                            _ -> toHtml settingValue

                                    td_ $ selectUI app.name appSetting

        handleChange :: ProgramName -> SettingKey -> SettingValue -> ProcessIO ()
        handleChange program setting value = do
            appSettings <- getAppSettings ctx.shared program
            case Map.lookup setting appSettings of
                Just prev
                    | prev == value -> pure ()
                    | otherwise -> do
                        logInfo "Changing setting" ["program" .= program, "setting" .= setting, "value" .= value]
                        atomically $ putSetting program setting (Just value) mvSettings
                        appInstances <- atomically $ getApps ctx.shared.apps
                        forM_ appInstances \appInstance -> do
                            when (appInstance.app.name == program) do
                                writePipe appInstance.pipe $ AppSettingChanged setting prev value
                        sendsHtml ctx.shared.clients mountUI
                Nothing -> logError "Unknown setting" ["program" .= program, "setting" .= setting]

    -- Handle events
    forever do
        atomically (readPipe ctx.pipe) >>= \case
            AppDisplay (UserJoined client) -> atomically $ sendHtml client mountUI
            AppTrigger ev -> do
                case (ev.body ^? key "program" . _JSON, ev.body ^? key "setting" . _JSON, ev.body ^? key "value" . _JSON) of
                    (Just program, Just setting, mValue) -> case (ev.trigger, mValue) of
                        ("set", Just value) -> handleChange program setting value
                        ("clear", Nothing) -> atomically $ putSetting program setting Nothing mvSettings
                        _ -> logError "Unknown trigger" ["ev" .= ev]
                    _ -> logError "Unknown settings" ["ev" .= ev]
                sendsHtml ctx.shared.clients mountUI
            ev -> logError "Unknown ev" ["ev" .= ev]
