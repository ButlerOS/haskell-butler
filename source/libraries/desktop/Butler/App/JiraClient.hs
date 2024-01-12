module Butler.App.JiraClient (jiraClientApp) where

import Butler

import Butler.App
import Butler.App.PokerPlanner (pokerPlannerApp)
import Butler.AppSettings

import Data.List (nub, sort, sortOn)
import Data.Map.Strict qualified as Map
import Jira qualified
import Network.HTTP.Client.TLS (newTlsManager)

jiraClientApp :: App
jiraClientApp =
    (defaultApp "jira-client" startJiraClient)
        { tags = fromList ["Development"]
        , description = "Manage Jira"
        , settings =
            [ AppSetting "jira-url" "" SettingURL
            , AppSetting "jira-token" "" SettingToken
            , AppSetting "jira-project" "" SettingName
            ]
        }

data Status = Loading | Stories [Jira.JiraIssue]

data Controls = Controls
    { byName :: Bool
    , noPoints :: Bool
    , typeFilter :: Maybe Text
    }
    deriving (Generic)

defaultControls :: Controls
defaultControls = Controls False False Nothing

data State = State
    { status :: Status
    , controls :: Controls
    , client :: Maybe Jira.JiraClient
    }
    deriving (Generic)

startJiraClient :: AppContext -> ProcessIO ()
startJiraClient ctx = do
    -- Setup state
    logInfo "JiraClient started!" []

    let getClient :: ProcessIO (Maybe Jira.JiraClient)
        getClient = do
            appSettings <- getAppSettings ctx.shared "jira-client"
            case (Map.lookup "jira-url" appSettings, Map.lookup "jira-token" appSettings) of
                (Just (into @Text -> url), Just (into @ByteString -> token))
                    | url /= mempty && token /= mempty ->
                        Just . Jira.newJiraClient url Nothing token <$> newTlsManager
                _ -> pure Nothing

        getProject :: ProcessIO (Maybe Text)
        getProject = do
            appSettings <- getAppSettings ctx.shared "jira-client"
            pure (into @Text <$> Map.lookup "jira-project" appSettings)

    tState <- newTVarIO . State Loading defaultControls =<< getClient

    -- UI
    let renderControls :: [Jira.JiraIssue] -> Controls -> HtmlT STM ()
        renderControls stories ctrls = with div_ [class_ "flex py-1 px-2 items-center"] do
            with div_ [class_ "grow"] mempty
            with div_ [class_ "px-3"] "Filters stories:"
            div_ do
                with label_ [class_ "text-sm"] "without points: "
                input_ $ butlerCheckbox ctx.wid "no-points" [] ctrls.noPoints Nothing
            div_ do
                with label_ [class_ "text-sm pl-3"] "by name: "
                input_ $ butlerCheckbox ctx.wid "by-name" [] ctrls.byName Nothing
            div_ do
                with label_ [class_ "text-sm pl-3"] "by type: "
                select_ [class_ "text-sm rounded", id_ "taskPrio", name_ "taskPrio", onChangeTrigger ctx.wid "filter"] $ do
                    let optionAttrs name
                            | name == "All" = case ctrls.typeFilter of
                                Nothing -> [selected_ ""]
                                Just _ -> []
                            | otherwise = case ctrls.typeFilter of
                                Just v | v == name -> [selected_ ""]
                                _ -> []
                    let mkOption name = option_ (optionAttrs name) (toHtml name)
                    mkOption "All"
                    forM_ (nub $ sort $ map (\issue -> issue.issueType) stories) mkOption

    let renderStories :: UTCTime -> Jira.JiraClient -> [Jira.JiraIssue] -> HtmlT STM ()
        renderStories now client stories = do
            ul_ do
                forM_ stories \x -> with li_ [class_ "flex items-center"] do
                    let jid = into @Text x.name
                    let startPlannerScript = startAppScript pokerPlannerApp ["argv" .= object ["requestor" .= ctx.wid, "story" .= jid]]
                    with button_ [class_ btnBlueClass, onclick_ startPlannerScript] "vote"
                    with span_ [class_ "w-[24px] text-right mx-1"] $ case x.score of
                        Nothing -> "?"
                        Just score -> toHtml (show @Int $ round score)
                    with a_ [href_ (Jira.jiraUrl client x.name), target_ "blank", class_ "cursor-pointer hover:font-bold text-blue-600"] (toHtml jid)
                    with span_ [class_ "ml-1 text-sm"] (toHtml (humanReadableTime' now x.updated))
                    ": "
                    with span_ [class_ "ml-1"] (toHtml x.summary)
            withTrigger_ "click" ctx.wid "refresh" button_ [class_ $ "mt-4 " <> btnGreenClass] "refresh"

    let applyControls ctrls =
            (if ctrls.noPoints then filter (\issue -> isNothing issue.score) else id)
                . (if ctrls.byName then sortOn (\issue -> issue.name) else id)
                . (maybe id (\expectedType -> filter (\issue -> issue.issueType == expectedType)) ctrls.typeFilter)

    let mountUI :: UTCTime -> HtmlT STM ()
        mountUI now = with div_ [wid_ ctx.wid "w", class_ "flex flex-col"] do
            state <- lift (readTVar tState)
            case state.client of
                Nothing -> do
                    "jira-url or jira-token is missing, set them using the setting app"
                Just client -> case state.status of
                    Loading -> "Loading..."
                    Stories stories -> with div_ [class_ "flex flex-col"] do
                        ctrls <- controls <$> lift (readTVar tState)
                        renderControls stories ctrls
                        renderStories now client (applyControls ctrls stories)

    let getStories :: Jira.JiraClient -> ProcessIO (Either Text [Jira.JiraIssue])
        getStories client = runExceptT @Text do
            -- now <- liftIO getCurrentTime
            -- pure
            --     [ Jira.JiraIssue "PROJ" "test-001" "story" now Nothing "desc" Nothing
            --     , Jira.JiraIssue "PROJ" "test-002" "epic" now Nothing "desc" (Just 42)
            --     ]

            project <-
                lift getProject >>= \case
                    Just project -> pure project
                    Nothing -> throwError "Project is missing"
            searchResult <-
                let searchAction = Jira.searchIssues client (Jira.JiraSearchRequest 0 100 $ Jira.JQL ("project = " <> project))
                 in lift (httpRetry 5 (liftIO searchAction)) >>= \case
                        Right res -> pure res
                        Left e -> throwError ("Query failed for project " <> project <> ": " <> e)
            let (errors, stories) = partitionEithers searchResult.issues
            unless (null errors) do
                lift $ logError "Jira decoding failures" ["errors" .= errors]
            pure stories

    -- Load stories
    baton <- newEmptyMVar
    let loadStories = tryPutMVar baton ()
    spawnThread_ $ forever do
        takeMVar baton
        atomically ((.client) <$> readTVar tState) >>= \case
            Just client -> do
                atomically $ modifyTVar' tState $ #status .~ Loading
                logInfo "Loading stories..." []
                now <- liftIO getCurrentTime
                sendsHtml ctx.shared.clients (mountUI now)
                eStories <- getStories client
                newStatus <- case eStories of
                    Left err -> do
                        logError "getStories failed" ["err" .= err]
                        pure Loading
                    Right stories -> pure (Stories stories)
                atomically $ modifyTVar' tState $ #status .~ newStatus
                sendsHtml ctx.shared.clients (mountUI now)
                void $ tryTakeMVar baton
            Nothing -> pure ()

    let setScore :: Jira.JiraID -> Float -> ProcessIO ()
        setScore jid score = do
            atomically ((.client) <$> readTVar tState) >>= \case
                Just client -> do
                    logInfo "Setting score" ["story" .= jid, "score" .= score]
                    httpRetry 5 (liftIO (Jira.setIssueScore client jid score)) >>= \case
                        Just err -> logError "Setting score failed" ["err" .= err, "story" .= jid]
                        Nothing -> pure ()
                Nothing -> logError "client is not configured" []

    mClient <- atomically ((.client) <$> readTVar tState)
    forM_ mClient (const loadStories)

    -- Handle events
    forever do
        aev <- atomically (readPipe ctx.pipe)
        now <- liftIO getCurrentTime
        case aev of
            AppDisplay (UserJoined client) -> atomically $ sendHtml client (mountUI now)
            AppSettingChanged{} ->
                getClient >>= \case
                    Just{} -> do
                        client <- getClient
                        atomically $ modifyTVar' tState (#client .~ client)
                        sendsHtml ctx.shared.clients (mountUI now)
                    Nothing -> pure ()
            AppTrigger ev -> do
                case ev.trigger of
                    "refresh" -> void $ tryPutMVar baton ()
                    "poker-result" ->
                        case (ev.body ^? key "value" . _JSON, ev.body ^? key "story" . _JSON, ev.body ^? key "wid" . _JSON) of
                            (Just v, Just story, Just wid) -> do
                                closeApp ctx.shared ev.client wid
                                setScore story v
                            res -> logError "Unknown result" ["ev" .= ev, "res" .= res]
                    "no-points" -> do
                        atomically $ modifyTVar' tState (#controls . #noPoints %~ not)
                    "by-name" -> do
                        atomically $ modifyTVar' tState (#controls . #byName %~ not)
                    "filter" -> do
                        case ev.body ^? key "value" . _String of
                            Just v -> do
                                let f = if v == "All" then Nothing else Just v
                                atomically $ modifyTVar' tState (#controls . #typeFilter .~ f)
                            Nothing -> logError "Unknown filter" ["ev" .= ev]
                    _ -> logError "Unknown trigger" ["ev" .= ev]
                sendsHtml ctx.shared.clients (mountUI now)
            ev -> logError "Unknown ev" ["ev" .= ev]
