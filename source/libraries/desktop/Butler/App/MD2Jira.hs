module Butler.App.MD2Jira (md2jiraApp) where

import Butler
import Butler.App
import Butler.App.JiraClient (JiraSetting (..), getJiraSetting)
import Butler.Core (writePipe)
import Butler.Core.NatMap qualified as NM
import Control.OperationalTransformation.Server (Revision)
import Data.List (sortOn)
import Jira (IssueData (..), jiraUrl)
import MD2Jira
import OT qualified

md2jiraApp :: App
md2jiraApp = defaultApp "md2jira" startMd2Jira

-- | The internal viewing state for epics and stories, see 'updateItemState'
data ItemState issue child = ItemState
    { itemID :: Natural
    , position :: Pos
    , issue :: issue
    , title :: Text
    , isExpanded :: Bool
    , childs :: NM.NatMap child
    }

type EpicState = ItemState Epic StoryState
type StoryState = ItemState Story TaskState
type TaskState = ItemState Task ()

data AppState = AppState
    { doc :: Text
    , rev :: Revision
    }

newtype Pos = Pos Natural
    deriving newtype (Eq, Ord, Num)

findItem :: Text -> [ItemState a b] -> (Maybe (ItemState a b), [ItemState a b])
findItem title = go []
  where
    go acc [] = (Nothing, reverse acc)
    go acc (item : rest)
        | item.title == title = (Just item, reverse acc <> rest)
        | otherwise = go (item : acc) rest

getChilds :: NM.NatMap (ItemState a b) -> STM [ItemState a b]
getChilds nm = sortOn (.position) <$> NM.elems nm

getTasks :: NM.NatMap EpicState -> STM [(EpicState, StoryState, TaskState)]
getTasks nmEpic = reverse <$> (foldM goEpic [] =<< NM.elems nmEpic)
  where
    goEpic acc epic = foldM (goStory epic) acc =<< NM.elems epic.childs
    goStory epic acc story = foldM (goTask epic story) acc =<< NM.elems story.childs
    goTask epic story acc task = pure ((epic, story, task) : acc)

getEpics :: NM.NatMap EpicState -> STM [Epic]
getEpics nmEpic = reverse <$> (foldM goEpic [] =<< NM.elems nmEpic)
  where
    goEpic acc epic = do
        stories <- reverse <$> (foldM goStory [] =<< NM.elems epic.childs)
        pure (epic.issue{stories} : acc)
    goStory acc story = do
        tasks <- reverse <$> (foldM goTask [] =<< NM.elems story.childs)
        pure (story.issue{tasks} : acc)
    goTask acc task = pure (task.issue : acc)

-- | Based on the current state and the md2jira parser output, this return updated ItemState for rendering purpose.
updateItemState :: NM.NatMap EpicState -> [Epic] -> STM ()
updateItemState nmEpic epics = goEpic 0 epics =<< NM.elems nmEpic
  where
    goEpic :: Pos -> [Epic] -> [EpicState] -> STM ()
    goEpic _ [] leftOver = do
        -- Remove deleted story. NM ensure the id always increase
        traverse_ (\is -> NM.delete nmEpic is.itemID) leftOver
    goEpic position (epic : epics') items = do
        let (mItem, leftOver) = findItem epic.info.summary items
        epicState <- case mItem of
            Nothing -> NM.addWithKey nmEpic (newEpic position epic)
            Just item -> update nmEpic position epic item
        goStory epicState.childs 0 epic.stories =<< NM.elems epicState.childs
        goEpic (position + 1) epics' leftOver

    goStory :: NM.NatMap StoryState -> Pos -> [Story] -> [StoryState] -> STM ()
    goStory nmStory _ [] leftOver = do
        traverse_ (\is -> NM.delete nmStory is.itemID) leftOver
    goStory nmStory position (story : stories) items = do
        let (mItem, leftOver) = findItem story.info.summary items
        storyState <- case mItem of
            Nothing -> NM.addWithKey nmStory (newStory position story)
            Just item -> update nmStory position story item
        goTask storyState.childs 0 story.tasks =<< NM.elems storyState.childs
        goStory nmStory (position + 1) stories leftOver

    goTask :: NM.NatMap TaskState -> Pos -> [Task] -> [TaskState] -> STM ()
    goTask nmTask _ [] leftOver = do
        traverse_ (\is -> NM.delete nmTask is.itemID) leftOver
    goTask nmTask position (task : tasks) items = do
        let (mItem, leftOver) = findItem task.info.summary items
        _ <- case mItem of
            Nothing -> NM.addWithKey nmTask (newTask position task)
            Just item -> update nmTask position task item
        goTask nmTask (position + 1) tasks leftOver

    update :: NM.NatMap (ItemState issue child) -> Pos -> issue -> ItemState issue child -> STM (ItemState issue child)
    update nm position issue item = do
        let newItem = item{position, issue}
        NM.insert nm item.itemID newItem
        pure newItem

    newEpic :: Pos -> Epic -> Natural -> STM EpicState
    newEpic pos epic = newItemState pos epic.info.summary epic

    newStory :: Pos -> Story -> Natural -> STM StoryState
    newStory pos story = newItemState pos story.info.summary story

    newTask :: Pos -> Task -> Natural -> STM TaskState
    newTask pos task = newItemState pos task.info.summary task

    newItemState :: Pos -> Text -> issue -> Natural -> STM (ItemState issue child)
    newItemState pos title issue itemID = ItemState itemID pos issue title True <$> NM.newNatMap

startMd2Jira :: AppContext -> ProcessIO ()
startMd2Jira ctx = do
    (vData :: TVar (Either String AppState)) <- newTVarIO (Left "no data...")
    vCache <- newTVarIO mempty
    nmState <- atomically NM.newNatMap

    -- Keep track of the Noter app to send update
    vNoterCtx <- newTVarIO Nothing

    -- TODO: handle reload when setting changes...
    mSetting <- getJiraSetting ctx.shared

    let
        -- Create stable id for dom
        epicID_ itemID = wid_ ctx.wid ("e-" <> showT itemID)
        storyID_ parentID itemID = wid_ ctx.wid ("e-" <> showT parentID <> "-" <> showT itemID)
        taskID_ (i1, i2) i3 = wid_ ctx.wid ("e-" <> showT i1 <> "-" <> showT i2 <> "-" <> showT i3)

        -- Render the expand button trigger
        expandButton :: ItemState a b -> [Pair] -> HtmlT STM ()
        expandButton itemState attrs =
            let trig = withTrigger "click" ctx.wid "toggle" attrs
                expandIcon = if itemState.isExpanded then "▼" else "▶"
             in trig i_ [class_ "cursor-pointer mr-1"] expandIcon

        renderTaskStatus :: (Natural, Natural) -> Natural -> Bool -> TaskStatus -> HtmlT STM ()
        renderTaskStatus (e, s) t reset status =
            let trig = withTrigger "click" ctx.wid "toggle" ["epic" .= e, "story" .= s, "task" .= t, "reset" .= reset]
                icon = case status of
                    Done -> "[x]"
                    InProgress -> "[.]"
                    _ -> "[ ]"
             in trig i_ [class_ "cursor-pointer mr-1"] icon

        -- Render JiraID link
        renderJira jiraID =
            let addLink = case mSetting of
                    Nothing -> id
                    Just setting -> mappend [href_ (Jira.jiraUrl setting.client jiraID), target_ "blank"]
             in with a_ (addLink [class_ "float-right cursor-pointer"]) (toHtml $ into @Text jiraID)

        -- Render task
        renderTask :: (Natural, Natural) -> Bool -> TaskState -> HtmlT STM ()
        renderTask parentID reset task = with div_ [taskID_ parentID task.itemID, class_ "mb-2 bg-slate-150"] do
            h3_ do
                renderTaskStatus parentID task.itemID reset task.issue.status
                toHtml task.issue.info.summary
                pre_ $ toHtml $ task.issue.info.description

        -- Render a story
        renderStory :: Natural -> StoryState -> HtmlT STM ()
        renderStory parentID story = with div_ [storyID_ parentID story.itemID, class_ "mb-2 bg-slate-200"] do
            forM_ story.issue.mJira renderJira
            h2_ do
                expandButton story ["epic" .= parentID, "story" .= story.itemID]
                toHtml story.issue.info.summary
            when story.isExpanded do
                with div_ [class_ "ml-4"] do
                    pre_ $ toHtml $ story.issue.info.description
                    mapM_ (renderTask (parentID, story.itemID) True) =<< lift (getChilds story.childs)
        -- Render a epic
        renderEpic :: EpicState -> HtmlT STM ()
        renderEpic epic = with div_ [epicID_ epic.itemID, class_ "border-b-4 border-indigo-500 pb-2 mb-2"] do
            forM_ epic.issue.mJira renderJira

            with h1_ [class_ "font-semibold"] do
                expandButton epic ["epic" .= epic.itemID]
                toHtml epic.issue.info.summary
            when epic.isExpanded do
                with div_ [class_ "ml-4"] do
                    pre_ $ toHtml $ epic.issue.info.description
                    mapM_ (renderStory epic.itemID) =<< lift (getChilds epic.childs)

        section = with h1_ [class_ "font-bold bg-slate-300"]

        renderTaskLists :: HtmlT STM ()
        renderTaskLists = with div_ [wid_ ctx.wid "tasks"] do
            tasks <- lift $ getTasks nmState
            let queued = filter (\(_, _, t) -> t.issue.status == InProgress) tasks
            unless (null queued) do
                section "Queue"
                forM_ queued \(epic, story, task) -> do
                    forM_ story.issue.mJira renderJira
                    renderTask (epic.itemID, story.itemID) False task

            let completed = filter (\(_, _, t) -> t.issue.status == Done) tasks
            unless (null completed) do
                section "Completed"
                forM_ completed \(_, story, task) -> do
                    forM_ story.issue.mJira renderJira
                    div_ do
                        "- [x] "
                        toHtml task.issue.info.summary
                        pre_ $ toHtml $ task.issue.info.description

        -- Create the UI
        mountUI = do
            with div_ [wid_ ctx.wid "w", class_ "my-1 mx-2"] do
                lift (readTVar vData) >>= \case
                    Left err -> div_ $ toHtml $ "Oops: " <> into @Text err
                    Right{} -> do
                        case mSetting of
                            Just{} -> withTrigger_ "click" ctx.wid "sync" button_ [class_ ("float-right " <> btnBlueClass)] "PUSH"
                            Nothing -> with button_ [class_ "float-right", disabled_ "", alt_ "Missing jira settings"] "missing jira-url, check the setting app"
                        div_ do
                            renderTaskLists
                            section "BackLog"
                            mapM_ renderEpic =<< lift (getChilds nmState)

        -- Toggle a story expand state
        toggleStory epicState storyState = do
            -- Just update the state and re-render the div element
            let newState = storyState{isExpanded = not storyState.isExpanded}
            atomically $ NM.insert epicState.childs storyState.itemID newState
            sendsHtml ctx.shared.clients $ renderStory epicState.itemID newState

        -- Toggle an epic expand state
        toggleEpic epicState = do
            let newState = epicState{isExpanded = not epicState.isExpanded}
            atomically $ NM.insert nmState epicState.itemID newState
            sendsHtml ctx.shared.clients $ renderEpic newState

        toggleTask epicState storyState taskState reset = withNoter \noterCtx -> withData \state -> do
            let newStatus = case taskState.issue.status of
                    Todo -> InProgress
                    Done -> Todo
                    InProgress -> case reset of
                        Just True -> Todo
                        _ -> Done
                newTask = taskState.issue{MD2Jira.status = newStatus}
                newState = taskState{issue = newTask}
            atomically $ NM.insert storyState.childs taskState.itemID newState
            -- update noter
            updateNoter noterCtx state =<< atomically (getEpics nmState)
            -- TODO: update Noter!
            sendsHtml ctx.shared.clients do
                renderTaskLists
                renderTask (epicState.itemID, storyState.itemID) True newState

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
                        writeTVar vData =<< case parse doc of
                            Left err -> pure (Left err)
                            Right epics' -> do
                                updateItemState nmState epics'
                                pure (Right (AppState{rev, doc}))
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
                                epics <- atomically $ getEpics nmState
                                logInfo "Syncing epics" ["epics" .= show epics]
                                cache <- readTVarIO vCache

                                (newEpics, newCache, errors) <- withRunInIO \run -> do
                                    let logger msg = run $ logInfo msg []
                                    eval logger setting.client setting.project epics cache
                                atomically $ writeTVar vCache newCache
                                unless (null errors) do
                                    logError "md2jira eval errors!" ["errors" .= errors]
                                -- Generate and send text operation to noter
                                updateNoter noterCtx state newEpics
                "toggle" | Just epicID <- ev.body ^? key "epic" . _JSON -> do
                    -- Update the view
                    atomically (NM.lookup nmState epicID) >>= \case
                        Nothing -> logError "Unknown epic" ["ev" .= ev]
                        Just epicState -> case ev.body ^? key "story" . _JSON of
                            Just storyID ->
                                atomically (NM.lookup epicState.childs storyID) >>= \case
                                    Just storyState -> case ev.body ^? key "task" . _JSON of
                                        Nothing -> toggleStory epicState storyState
                                        Just taskID ->
                                            atomically (NM.lookup storyState.childs taskID) >>= \case
                                                Just taskState -> toggleTask epicState storyState taskState (ev.body ^? key "reset" . _JSON)
                                                Nothing -> logError "Unknown task" ["ev" .= ev]
                                    Nothing -> logError "Unknown story" ["ev" .= ev]
                            Nothing -> toggleEpic epicState
                _ -> logError "Unknown ev" ["ev" .= ev]
            _ -> pure ()
