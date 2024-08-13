-- no-orphans for md2jira serialise instances
{-# OPTIONS_GHC -Wno-orphans #-}

module Butler.App.MD2Jira (md2jiraApp) where

import Butler
import Butler.App
import Butler.App.JiraClient (JiraSetting (..), getJiraSetting, setScore)
import Butler.App.PokerPlanner (pokerPlannerApp)
import Butler.Core (writePipe)
import Control.OperationalTransformation.Server (Revision)
import Data.List (foldl')
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Jira (IssueData (..), JiraID, jiraUrl)
import MD2Jira
import OT qualified

md2jiraApp :: App
md2jiraApp = defaultApp "md2jira" startMd2Jira

data AppState = AppState
    { doc :: Text
    , rev :: Revision
    , epics :: [Epic]
    , status :: [Text]
    }
initialState :: AppState
initialState = AppState "" (-1) [] ["Waiting for data..."]

getTasks :: [Epic] -> [(Story, Task)]
getTasks = concatMap goEpic
  where
    goEpic epic = concatMap goStory epic.stories
    goStory story = map (goTask story) story.tasks
    goTask story task = (story, task)

updateScore :: Float -> JiraID -> [Epic] -> [Epic]
updateScore score jid = map goEpic
  where
    goEpic epic = epic{stories = map goStory epic.stories}
    goStory story
        | story.mJira == Just jid = story{mScore = Just score}
        | otherwise = story

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
    vState <- newTVarIO initialState

    vCache <- snd <$> newProcessMemory "cache" (pure mempty)
    vExpandedState <- newTVarIO mempty

    -- Keep track of the Noter app to send update
    vNoterCtx <- newTVarIO Nothing

    -- TODO: handle reload when setting changes...
    mSetting <- getJiraSetting ctx.shared

    let
        scroll :: AppID -> Text -> Text
        scroll quill target = "event.stopPropagation(); scrollQuill(" <> showT quill <> ", \"" <> target <> "\")"

        isExpanded :: Map JiraID Bool -> Maybe JiraID -> Bool
        isExpanded _ Nothing = False
        isExpanded m (Just jid) = fromMaybe False (Map.lookup jid m)

        addJID :: Maybe JiraID -> _ -> _
        addJID = \case
            Nothing -> id
            Just jid -> (wid_ ctx.wid (from jid) :)

        addScroll :: AppID -> Maybe JiraID -> _ -> _
        addScroll quill = \case
            Nothing -> id
            Just jid -> (onclick_ (scroll quill (from jid)) :)

        -- Render the expand button trigger
        expandButton :: Map JiraID Bool -> JiraID -> HtmlT STM ()
        expandButton expandedState jid =
            let trig = withTrigger "click" ctx.wid "toggle" ["jid" .= jid]
                expandIcon = if isExpanded expandedState (Just jid) then "▼" else "▶"
             in trig i_ [class_ "cursor-pointer mr-1"] expandIcon

        voteButton :: JiraID -> HtmlT STM ()
        voteButton jid = with button_ [class_ btnBlueClass, onclick_ startPokerScript] "vote"
          where
            startPokerScript = startAppScript pokerPlannerApp ["argv" .= object ["requestor" .= ctx.wid, "story" .= jid]]

        renderTaskStatus :: TaskStatus -> HtmlT STM ()
        renderTaskStatus status = with i_ [class_ "mr-1"] $
            case status of
                Done -> "[x]"
                InProgress{assigned} -> toHtml assigned <> ":"
                Todo -> "[ ]"

        -- Render JiraID link
        renderJira jiraID =
            let addLink = case mSetting of
                    Nothing -> id
                    Just setting -> mappend [href_ (Jira.jiraUrl setting.client jiraID), target_ "blank"]
             in with a_ (addLink [class_ "float-right cursor-pointer"]) (toHtml $ into @Text jiraID)

        -- Render task
        renderTask :: AppID -> Task -> HtmlT STM ()
        renderTask quill task = with div_ [class_ "mb-2 bg-slate-150 flex", onclick_ (scroll quill task.info.summary)] do
            div_ do
                renderTaskStatus task.status
            div_ do
                toHtml $ T.strip task.info.summary
                pre_ $ toHtml $ T.strip task.info.description

        -- Render a story
        renderStory :: AppID -> Map JiraID Bool -> Story -> HtmlT STM ()
        renderStory quill expandedState story = with div_ (addJID story.mJira [class_ "mb-2 bg-slate-200"]) do
            forM_ story.mJira renderJira
            with span_ (addScroll quill story.mJira []) do
                h2_ do
                    let go (tot, done) task = (tot + 1, done + if task.status == Done then 1 else 0)
                    let (total, completed) = foldl' go (0 :: Word, 0 :: Word) story.tasks
                    when (total > 0 && completed > 0) do
                        let percent = completed * 100 `div` total
                        with span_ [class_ "float-right font-bold mr-2", title_ "completion"] do
                            toHtml $ into @Text $ show percent <> "%"
                    with span_ [class_ "float-right mr-4"] do
                        case story.mScore of
                            Nothing -> forM_ story.mJira voteButton
                            Just score -> with span_ [class_ "text-xs", title_ "story points"] do toHtml $ showScore score
                    forM_ story.mJira (expandButton expandedState)
                    toHtml $ T.strip story.info.summary
                when (isExpanded expandedState story.mJira) do
                    with div_ [class_ "ml-4"] do
                        pre_ $ toHtml $ T.strip story.info.description
                        mapM_ (renderTask quill) story.tasks

        -- Render a epic
        renderEpic :: AppID -> Map JiraID Bool -> Epic -> HtmlT STM ()
        renderEpic quill expandedState epic = with div_ (addJID epic.mJira [class_ "pb-2 mb-2"]) do
            forM_ epic.mJira renderJira
            with span_ (addScroll quill epic.mJira []) do
                with h1_ [class_ "font-semibold"] do
                    forM_ epic.mJira (expandButton expandedState)
                    toHtml $ T.strip epic.info.summary
                when (isExpanded expandedState epic.mJira) do
                    with div_ [class_ "ml-4"] do
                        pre_ $ toHtml $ T.strip epic.info.description
                        mapM_ (renderStory quill expandedState) epic.stories

        section (name :: HtmlT STM ()) xs =
            with h1_ [class_ "font-bold bg-slate-300 flex mt-4"] do
                with span_ [class_ "w-6 mr-2 block text-right"] $ toHtml (showT $ length xs)
                name

        renderTaskLists :: AppID -> [(Story, Task)] -> HtmlT STM ()
        renderTaskLists quill tasks = with div_ [wid_ ctx.wid "tasks"] do
            let isInprogress = \case
                    InProgress "." -> False
                    InProgress _ -> True
                    _ -> False
            let queued = filter (\(_, t) -> isInprogress t.status) tasks
            unless (null queued) do
                section "Queue" queued
                forM_ queued \(story, task) -> do
                    forM_ story.mJira renderJira
                    when (isNothing story.mScore) do
                        with span_ [class_ "font-bold float-right text-red-600", title_ "Story has no points!"] "⚠"
                    renderTask quill task

            let isNext = \case
                    InProgress "." -> True
                    _ -> False
            let next = filter (\(_, t) -> isNext t.status) tasks
            unless (null next) do
                section "Next" next
                forM_ next \(story, task) -> do
                    forM_ story.mJira renderJira
                    with div_ (addScroll quill story.mJira []) do
                        "[.] "
                        toHtml $ T.strip task.info.summary

            let completed = filter (\(_, t) -> t.status == Done) tasks
            unless (null completed) do
                section "Completed" completed
                forM_ completed \(story, task) -> do
                    forM_ story.mJira renderJira
                    with div_ (addScroll quill story.mJira []) do
                        "[x] "
                        toHtml $ T.strip task.info.summary
                        pre_ $ toHtml $ T.strip task.info.description

        renderStatus rev status = with div_ [wid_ ctx.wid "s"] do
            case status of
                [] -> do
                    with span_ [class_ "mr-2"] $ toHtml $ "Rev: " <> show rev
                    let errMsg = "missing jira-url, check the setting app"
                    case mSetting of
                        Just{} -> withTrigger_ "click" ctx.wid "sync" button_ [class_ $ "float-right " <> btnBlueClass] "PUSH"
                        Nothing -> with button_ [disabled_ "", alt_ "Missing jira settings"] errMsg
                _ -> do
                    with div_ [class_ "pb-6"] do
                        mapM_ (pre_ . toHtml) status

        -- Create the UI
        mountUI = do
            with div_ [wid_ ctx.wid "w", class_ "my-1 mx-2"] do
                state <- lift (readTVar vState)
                quill <- maybe shellAppID (.wid) <$> lift (readTVar vNoterCtx)
                renderStatus state.rev state.status
                div_ do
                    let tasks = getTasks state.epics
                    renderTaskLists quill tasks
                    section "BackLog" $ filter (\(_, t) -> t.status == Todo) tasks
                    expandedState <- lift (readTVar vExpandedState)
                    mapM_ (renderEpic quill expandedState) state.epics

        expandJID quill jid issue = do
            expandedState <- atomically do
                m <- readTVar vExpandedState
                let newState
                        | isExpanded m (Just jid) = False
                        | otherwise = True
                    newM = Map.insert jid newState m
                writeTVar vExpandedState newM
                pure newM
            sendsHtml ctx.shared.clients $ case issue of
                Left epic -> renderEpic quill expandedState epic
                Right story -> renderStory quill expandedState story

        withNoter cb =
            readTVarIO vNoterCtx >>= \case
                Nothing -> logError "No Noter Application for syncing..." []
                Just noterCtx -> cb noterCtx
        withData cb = do
            state <- readTVarIO vState
            case state.status of
                [] -> cb state
                err -> logError "Can't sync because of error" ["err" .= err]

        updateNoter noterCtx state newEpics = do
            case OT.updateDoc state.doc (printer newEpics) of
                Nothing -> sendsHtml ctx.shared.clients $ renderStatus state.rev state.status
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
                    atomically do
                        prevState <- readTVar vState
                        writeTVar vState $ case parse "jira.md" doc of
                            Left err -> prevState{Butler.App.MD2Jira.status = [into err]}
                            Right epics -> prevState{rev, doc, epics, status = []}
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
                                sendsHtml ctx.shared.clients $ with div_ [wid_ ctx.wid "s"] do
                                    "Start syncing..."
                                cache <- atomically $ readMemoryVar vCache

                                (newEpics, newCache, errors) <- withRunInIO \run -> do
                                    let logger msg = run do
                                            logInfo msg []
                                            sendsHtml ctx.shared.clients $
                                                with div_ [wid_ ctx.wid "s", hxSwapOob_ "beforeend"] do
                                                    pre_ do toHtml msg

                                    eval logger setting.client setting.project state.epics cache
                                when (newCache /= cache) do
                                    atomically $ modifyMemoryVar vCache (const newCache)
                                unless (null errors) do
                                    -- TODO: send the errors to the client
                                    logError "md2jira eval errors!" ["errors" .= errors]
                                -- Generate and send text operation to noter
                                updateNoter noterCtx state newEpics
                "toggle" | Just jid <- ev.body ^? key "jid" . _JSON -> withNoter \noterCtx -> withData \state ->
                    case findIssue state.epics jid of
                        Nothing -> logError "unknown toggle" ["jid" .= jid]
                        Just issue -> expandJID noterCtx.wid jid issue
                "poker-result" ->
                    case (ev.body ^? key "value" . _JSON, ev.body ^? key "story" . _JSON, ev.body ^? key "wid" . _JSON) of
                        (Just score, Just jid, Just wid) -> withNoter \noterCtx -> withData \state -> do
                            closeApp ctx.shared ev.client wid
                            forM_ mSetting \setting -> do
                                setScore setting jid score >>= \case
                                    True -> updateNoter noterCtx state $ updateScore score jid state.epics
                                    False -> sendsHtml ctx.shared.clients $ with div_ [wid_ ctx.wid "s"] do
                                        "Vote failed to sync on JIRA"
                        res -> logError "Unknown result" ["ev" .= ev, "res" .= res]
                _ -> logError "Unknown ev" ["ev" .= ev]
            _ -> pure ()

instance Serialise Task
instance Serialise TaskStatus
instance Serialise IssueData
instance Serialise JiraID
