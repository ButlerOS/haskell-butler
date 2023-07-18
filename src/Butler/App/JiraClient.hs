module Butler.App.JiraClient (jiraClientApp) where

import Butler

import Butler.App
import Butler.App.PokerPlanner (pokerPlannerApp)
import Butler.AppSettings

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

data State = State
    { status :: Status
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

    tState <- newTVarIO . State Loading =<< getClient

    -- UI
    let renderStories :: [Jira.JiraIssue] -> HtmlT STM ()
        renderStories xs = do
            ul_ do
                forM_ xs \x -> with li_ [class_ "flex items-center"] do
                    let jid = into @Text x.name
                    let startPlannerScript = startAppScript pokerPlannerApp ["argv" .= object ["requestor" .= ctx.wid, "story" .= jid]]
                    with button_ [class_ $ btnBlueClass, onclick_ startPlannerScript] "vote"
                    with span_ [class_ $ "w-[24px] text-right mx-1"] $ case x.score of
                      Nothing -> "?"
                      Just score -> toHtml (show @Int $ round score)
                    span_ (toHtml jid)
                    ": "
                    with span_ [class_ "ml-1"] (toHtml x.summary)
            withTrigger_ "click" ctx.wid "refresh" button_ [class_ $ "mt-4 " <> btnGreenClass] "refresh"

    let mountUI :: HtmlT STM ()
        mountUI = with div_ [wid_ ctx.wid "w", class_ "flex flex-col"] do
            state <- lift (readTVar tState)
            case state.client of
                Nothing -> do
                    "jira-url or jira-token is missing, set them using the setting app"
                Just{} -> case state.status of
                    Loading -> "Loading..."
                    Stories xs -> renderStories xs

    let getStories :: Jira.JiraClient -> ProcessIO (Either Text [Jira.JiraIssue])
        getStories client = runExceptT @Text do
            -- now <- liftIO getCurrentTime
            -- pure [Jira.JiraIssue "PROJ" "test-001" "story" now Nothing "desc" Nothing,
            --      Jira.JiraIssue "PROJ" "test-002" "story" now Nothing "desc" (Just 42)]
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
                sendsHtml ctx.shared.clients mountUI
                eStories <- getStories client
                newStatus <- case eStories of
                    Left err -> do
                        logError "getStories failed" ["err" .= err]
                        pure Loading
                    Right stories -> pure (Stories stories)
                atomically $ modifyTVar' tState $ #status .~ newStatus
                sendsHtml ctx.shared.clients mountUI
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
        atomically (readPipe ctx.pipe) >>= \case
            AppDisplay (UserJoined client) -> atomically $ sendHtml client mountUI
            AppSettingChanged{} ->
                getClient >>= \case
                    Just{} -> do
                        client <- getClient
                        atomically $ modifyTVar' tState (#client .~ client)
                        sendsHtml ctx.shared.clients mountUI
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
                    _ -> logError "Unknown trigger" ["ev" .= ev]
                sendsHtml ctx.shared.clients mountUI
            ev -> logError "Unknown ev" ["ev" .= ev]
