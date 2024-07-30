module Butler.App.MD2Jira (md2jiraApp) where

import Butler
import Butler.App
import Butler.App.JiraClient (JiraSetting (..), getJiraSetting)
import Butler.Core (writePipe)
import Control.OperationalTransformation.Server (Revision)
import Data.List (foldl')
import Data.Map.Strict qualified as Map
import Jira (IssueData (..), JiraID, jiraUrl)
import MD2Jira
import OT qualified

md2jiraApp :: App
md2jiraApp = defaultApp "md2jira" startMd2Jira

data AppState = AppState
    { doc :: Text
    , rev :: Revision
    , epics :: [Epic]
    }

getTasks :: [Epic] -> [(Story, Task)]
getTasks = concatMap goEpic
  where
    goEpic epic = concatMap goStory epic.stories
    goStory story = map (goTask story) story.tasks
    goTask story task = (story, task)

findIssue :: [Epic] -> JiraID -> Maybe (Either Epic Story)
findIssue epics jid = goEpics epics
  where
    goEpics [] = Nothing
    goEpics (epic : rest)
        | Just ejid <- epic.mJira, ejid == jid = Just (Left epic)
        | otherwise = case goStories epic.stories of
            Nothing -> goEpics rest
            Just story -> Just (Right story)
    goStories [] = Nothing
    goStories (story : rest)
        | Just sjid <- story.mJira, sjid == jid = Just story
        | otherwise = goStories rest

startMd2Jira :: AppContext -> ProcessIO ()
startMd2Jira ctx = do
    (vData :: TVar (Either String AppState)) <- newTVarIO (Left "no data...")
    vCache <- newTVarIO mempty
    vExpandedState <- newTVarIO mempty

    -- Keep track of the Noter app to send update
    vNoterCtx <- newTVarIO Nothing

    -- TODO: handle reload when setting changes...
    mSetting <- getJiraSetting ctx.shared

    let
        isExpanded :: Map JiraID Bool -> Maybe JiraID -> Bool
        isExpanded _ Nothing = True
        isExpanded m (Just jid) = fromMaybe True (Map.lookup jid m)

        addJID :: Maybe JiraID -> _ -> _
        addJID = \case
            Nothing -> id
            Just jid -> (wid_ ctx.wid (from jid) :)

        -- Render the expand button trigger
        expandButton :: Map JiraID Bool -> JiraID -> HtmlT STM ()
        expandButton expandedState jid =
            let trig = withTrigger "click" ctx.wid "toggle" ["jid" .= jid]
                expandIcon = if isExpanded expandedState (Just jid) then "▼" else "▶"
             in trig i_ [class_ "cursor-pointer mr-1"] expandIcon

        renderTaskStatus :: TaskStatus -> HtmlT STM ()
        renderTaskStatus status = with i_ [class_ "mr-1"] $
            case status of
                Done -> "[x]"
                InProgress{assigned} -> "[" <> toHtml assigned <> "]"
                Todo -> "[ ]"

        -- Render JiraID link
        renderJira jiraID =
            let addLink = case mSetting of
                    Nothing -> id
                    Just setting -> mappend [href_ (Jira.jiraUrl setting.client jiraID), target_ "blank"]
             in with a_ (addLink [class_ "float-right cursor-pointer"]) (toHtml $ into @Text jiraID)

        -- Render task
        renderTask :: Task -> HtmlT STM ()
        renderTask task = with div_ [class_ "mb-2 bg-slate-150"] do
            h3_ do
                renderTaskStatus task.status
                toHtml task.info.summary
                pre_ $ toHtml $ task.info.description

        -- Render a story
        renderStory :: Map JiraID Bool -> Story -> HtmlT STM ()
        renderStory expandedState story = with div_ (addJID story.mJira [class_ "mb-2 bg-slate-200"]) do
            forM_ story.mJira renderJira
            h2_ do
                let go (tot, done) task = (tot + 1, done + if task.status == Done then 1 else 0)
                let (total, completed) = foldl' go (0 :: Word, 0 :: Word) story.tasks
                let percent = completed * 100 `div` total
                when (percent > 0) do
                    with span_ [class_ "float-right font-bold mr-2"] do
                        toHtml $ into @Text $ show percent <> "%"
                forM_ story.mJira (expandButton expandedState)
                toHtml story.info.summary
            when (isExpanded expandedState story.mJira) do
                with div_ [class_ "ml-4"] do
                    pre_ $ toHtml $ story.info.description
                    mapM_ renderTask story.tasks

        -- Render a epic
        renderEpic :: Map JiraID Bool -> Epic -> HtmlT STM ()
        renderEpic expandedState epic = with div_ (addJID epic.mJira [class_ "pb-2 mb-2"]) do
            forM_ epic.mJira renderJira

            with h1_ [class_ "font-semibold"] do
                forM_ epic.mJira (expandButton expandedState)
                toHtml epic.info.summary
            when (isExpanded expandedState epic.mJira) do
                with div_ [class_ "ml-4"] do
                    pre_ $ toHtml $ epic.info.description
                    mapM_ (renderStory expandedState) epic.stories

        section = with h1_ [class_ "font-bold bg-slate-300"]

        renderTaskLists :: [Epic] -> HtmlT STM ()
        renderTaskLists epics = with div_ [wid_ ctx.wid "tasks"] do
            let tasks = getTasks epics
            let isInprogress = \case
                    InProgress{} -> True
                    _ -> False
            let queued = filter (\(_, t) -> isInprogress t.status) tasks
            unless (null queued) do
                section "Queue"
                forM_ queued \(story, task) -> do
                    forM_ story.mJira renderJira
                    renderTask task

            let completed = filter (\(_, t) -> t.status == Done) tasks
            unless (null completed) do
                section "Completed"
                forM_ completed \(story, task) -> do
                    forM_ story.mJira renderJira
                    div_ do
                        "- [x] "
                        toHtml task.info.summary
                        pre_ $ toHtml $ task.info.description

        -- Create the UI
        mountUI = do
            with div_ [wid_ ctx.wid "w", class_ "my-1 mx-2"] do
                lift (readTVar vData) >>= \case
                    Left err -> div_ $ toHtml $ "Oops: " <> into @Text err
                    Right state -> do
                        div_ do
                            renderTaskLists state.epics
                            section "BackLog"
                            expandedState <- lift (readTVar vExpandedState)
                            mapM_ (renderEpic expandedState) state.epics
                        div_ [wid_ ctx.wid "logs"] mempty
                        let errMsg = "missing jira-url, check the setting app"
                        case mSetting of
                            Just{} -> withTrigger_ "click" ctx.wid "sync" button_ [class_ btnBlueClass] "PUSH"
                            Nothing -> with button_ [disabled_ "", alt_ "Missing jira settings"] errMsg

        expandJID jid issue = do
            expandedState <- atomically do
                m <- readTVar vExpandedState
                let newState
                        | isExpanded m (Just jid) = False
                        | otherwise = True
                    newM = Map.insert jid newState m
                writeTVar vExpandedState newM
                pure newM
            sendsHtml ctx.shared.clients $ case issue of
                Left epic -> renderEpic expandedState epic
                Right story -> renderStory expandedState story

        withNoter cb =
            readTVarIO vNoterCtx >>= \case
                Nothing -> logError "No Noter Application for syncing..." []
                Just noterCtx -> cb noterCtx
        withData cb =
            readTVarIO vData >>= \case
                Left err -> logError "Can't sync error" ["err" .= err]
                Right s -> cb s

        updateNoter noterCtx state newEpics = do
            case OT.updateDoc state.doc (printer newEpics) of
                Nothing -> pure ()
                Just ops -> do
                    mvReply <- newEmptyTMVarIO
                    let msg = toDyn (state.rev, ops)
                    writePipe noterCtx.pipe (AppSync (SyncEvent "update-doc" msg mvReply))

    forever do
        atomically (readPipe ctx.pipe) >>= \case
            AppDisplay (UserJoined client) -> atomically $ sendHtml client mountUI
            AppSync es -> case es.name of
                -- Connecting to a noter instance
                "start-repl" | Just (noterCtx, _) <- fromDynamic @(AppContext, [String]) es.message -> do
                    atomically $ writeTVar vNoterCtx (Just noterCtx)
                    atomically (putTMVar es.reply (toDyn ()))
                -- The markdown was updated
                "new-doc" | Just (rev, doc) <- fromDynamic @(Revision, Text) es.message -> do
                    -- parse and update the state
                    atomically $
                        writeTVar vData $ case parse doc of
                            Left err -> Left err
                            Right epics -> Right (AppState{rev, doc, epics})
                    -- update the UI
                    sendsHtml ctx.shared.clients mountUI
                _ -> logError "Bad sync" ["action" .= es.name]
            AppTrigger ev -> case ev.trigger of
                "sync" ->
                    -- Push the document to JIRA
                    withNoter \noterCtx -> withData \state -> do
                        case mSetting of
                            Nothing -> pure ()
                            Just setting -> do
                                logInfo "Syncing epics" ["epics" .= show state.epics]
                                cache <- readTVarIO vCache

                                (newEpics, newCache, errors) <- withRunInIO \run -> do
                                    let logger msg = run do
                                            logInfo msg []
                                            sendsHtml ctx.shared.clients $
                                                with div_ [wid_ ctx.wid "logs", hxSwapOob_ "beforeend"] do
                                                    pre_ do toHtml msg

                                    eval logger setting.client setting.project state.epics cache
                                atomically $ writeTVar vCache newCache
                                unless (null errors) do
                                    logError "md2jira eval errors!" ["errors" .= errors]
                                -- Generate and send text operation to noter
                                updateNoter noterCtx state newEpics
                "toggle" | Just jid <- ev.body ^? key "jid" . _JSON -> withData \state ->
                    case findIssue state.epics jid of
                        Nothing -> logError "unknown toggle" ["jid" .= jid]
                        Just issue -> expandJID jid issue
                _ -> logError "Unknown ev" ["ev" .= ev]
            _ -> pure ()
