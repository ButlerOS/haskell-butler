module Butler.App.MD2Jira (md2jiraApp) where

import Butler
import Butler.App
import Butler.Core (writePipe)
import Butler.Core.NatMap qualified as NM
import Control.OperationalTransformation.Server (Revision)
import Data.List (find)
import Jira (IssueData (..))
import MD2Jira
import OT qualified

md2jiraApp :: App
md2jiraApp = defaultApp "md2jira" startMd2Jira

-- | The internal viewing state for epics and stories, see 'updateItemState'
data ItemState issue child = ItemState
    { itemID :: Natural
    , issue :: issue
    , title :: Text
    , isExpanded :: Bool
    , childs :: NM.NatMap child
    }

type EpicState = ItemState Epic StoryState
type StoryState = ItemState Story ()

data AppState = AppState
    { doc :: Text
    , rev :: Revision
    , epics :: [(EpicState, [StoryState])]
    }

findItem :: Text -> [ItemState a b] -> (Maybe (ItemState a b), [ItemState a b])
findItem title = go []
  where
    go acc [] = (Nothing, reverse acc)
    go acc (item : rest)
        | item.title == title = (Just item, reverse acc <> rest)
        | otherwise = go (item : acc) rest

-- | Based on the current state and the md2jira parser output, this return updated ItemState for rendering purpose.
updateItemState :: NM.NatMap EpicState -> [Epic] -> STM [(EpicState, [StoryState])]
updateItemState nmEpic epics = goEpic [] epics =<< NM.elems nmEpic
  where
    goEpic :: [(EpicState, [StoryState])] -> [Epic] -> [EpicState] -> STM [(EpicState, [StoryState])]
    goEpic acc [] leftOver = do
        -- Remove deleted story. NM ensure the id always increase
        traverse_ (\is -> NM.delete nmEpic is.itemID) leftOver
        pure $ reverse acc
    goEpic acc (epic : epics') items = do
        let (mItem, leftOver) = findItem epic.info.summary items
        epicState <- case mItem of
            Nothing -> NM.addWithKey nmEpic (newEpic epic)
            Just item -> pure $ item{issue = epic}
        storiesState <- goStory epicState.childs [] epic.stories =<< NM.elems epicState.childs
        goEpic ((epicState, storiesState) : acc) epics' leftOver

    goStory :: NM.NatMap StoryState -> [StoryState] -> [Story] -> [StoryState] -> STM [StoryState]
    goStory nmStory acc [] leftOver = do
        traverse_ (\is -> NM.delete nmStory is.itemID) leftOver
        pure $ reverse acc
    goStory nmStory acc (story : stories) items = do
        let (mItem, leftOver) = findItem story.info.summary items
        storyState <- case mItem of
            Nothing -> NM.addWithKey nmStory (newStory story)
            Just item -> pure $ item{issue = story}
        goStory nmStory (storyState : acc) stories leftOver

    newEpic :: Epic -> Natural -> STM EpicState
    newEpic epic = newItemState epic.info.summary epic

    newStory :: Story -> Natural -> STM StoryState
    newStory story = newItemState story.info.summary story

    newItemState :: Text -> issue -> Natural -> STM (ItemState issue child)
    newItemState title issue itemID = ItemState itemID issue title True <$> NM.newNatMap

startMd2Jira :: AppContext -> ProcessIO ()
startMd2Jira ctx = do
    (vData :: TVar (Either String AppState)) <- newTVarIO (Left "no data...")
    nmState <- atomically NM.newNatMap

    let
        -- Create stable id for dom
        epicID_ itemID = wid_ ctx.wid ("e-" <> showT itemID)
        storyID_ parentID itemID = wid_ ctx.wid ("e-" <> showT parentID <> "-" <> showT itemID)

        -- Render the expand button trigger
        expandButton :: ItemState a b -> [Pair] -> HtmlT STM ()
        expandButton itemState attrs =
            let trig = withTrigger "click" ctx.wid "toggle-expand" attrs
                expandIcon = case itemState.isExpanded of
                    True -> "▼"
                    False -> "▶"
             in trig i_ [class_ "cursor-pointer mr-1"] expandIcon

        -- Render JiraID link
        renderJira jiraID =
            -- TODO: set the href
            with a_ [class_ "float-right cursor-pointer"] (toHtml $ into @Text jiraID)

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

        -- Render a epic
        renderEpic :: (EpicState, [StoryState]) -> HtmlT STM ()
        renderEpic (epic, stories) = with div_ [epicID_ epic.itemID, class_ "border-b-4 border-indigo-500 pb-2 mb-2"] do
            forM_ epic.issue.mJira renderJira

            with h1_ [class_ "font-semibold"] do
                expandButton epic ["epic" .= epic.itemID]
                toHtml epic.issue.info.summary
            when epic.isExpanded do
                with div_ [class_ "ml-4"] do
                    pre_ $ toHtml $ epic.issue.info.description
                    mapM_ (renderStory epic.itemID) stories

        -- Create the UI
        mountUI = do
            with div_ [wid_ ctx.wid "w", class_ "my-1 mx-2"] do
                lift (readTVar vData) >>= \case
                    Left err -> div_ $ toHtml $ "Oops: " <> into @Text err
                    Right state -> do
                        withTrigger_ "click" ctx.wid "sync" button_ [class_ ("float-right " <> btnBlueClass)] "SYNC"
                        div_ do
                            let section = with h1_ [class_ "font-bold bg-slate-300"]
                            {-
                            section "Queue"
                            div_ "Task 1"
                            div_ "Task 2"
                            section "Completed"
                            div_ "Task 3"
                            -}
                            section "BackLog"
                            mapM_ renderEpic state.epics

        -- Toggle a story expand state
        toggleStory epicState storyState = do
            -- Just update the state and re-render the div element
            let newState = storyState{isExpanded = not storyState.isExpanded}
            atomically $ NM.insert epicState.childs storyState.itemID newState
            sendsHtml ctx.shared.clients $ renderStory epicState.itemID newState

        -- Toggle an epic expand state
        toggleEpic epicState = do
            let newState = epicState{isExpanded = not epicState.isExpanded}
            mStories <-
                if epicState.isExpanded
                    then -- To collapse an epic, we don't need the list of story
                        pure $ Just []
                    else -- To expand an epic, retrieve the list of story

                        readTVarIO vData >>= \case
                            Left{} -> pure Nothing
                            Right state -> case find (\(es, _) -> es.itemID == epicState.itemID) state.epics of
                                Nothing -> pure Nothing
                                Just (_, stories) -> pure $ Just stories
            case mStories of
                Nothing -> logError "Couldn't find epic" ["id" .= epicState.itemID, "title" .= epicState.title]
                Just stories -> do
                    atomically $ NM.insert nmState epicState.itemID newState
                    sendsHtml ctx.shared.clients $ renderEpic (newState, stories)

    -- Keep track of the Noter app to send update
    vNoterCtx <- newTVarIO Nothing
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
                                epics <- updateItemState nmState epics'
                                pure (Right (AppState{rev, doc, epics}))
                    -- update the UI
                    sendsHtml ctx.shared.clients mountUI
                _ -> logError "Bad sync" ["action" .= es.name]
            AppTrigger ev -> case ev.trigger of
                "sync" ->
                    -- Push the document to JIRA
                    readTVarIO vNoterCtx >>= \case
                        Nothing -> logError "No Noter Application for syncing..." []
                        Just noterCtx ->
                            readTVarIO vData >>= \case
                                Left err -> logError "Can't sync error" ["err" .= err]
                                Right state -> do
                                    mvReply <- newEmptyTMVarIO
                                    let epics = map (.issue) $ map fst state.epics
                                    -- TODO: call eval to inject JiraID!
                                    newEpics <- pure epics

                                    -- Generate and send text operation to noter
                                    let newDoc = printer newEpics
                                    let ops = OT.updateDoc state.doc newDoc
                                    let msg = toDyn (state.rev, ops)
                                    writePipe noterCtx.pipe (AppSync (SyncEvent "update-doc" msg mvReply))
                "toggle-expand" | Just epicID <- ev.body ^? key "epic" . _JSON -> do
                    -- Update the view
                    atomically (NM.lookup nmState epicID) >>= \case
                        Nothing -> logError "Unknown epic" ["ev" .= ev]
                        Just epicState -> case ev.body ^? key "story" . _JSON of
                            Just storyID ->
                                atomically (NM.lookup epicState.childs storyID) >>= \case
                                    Nothing -> logError "Unknown story" ["ev" .= ev]
                                    Just storyState -> toggleStory epicState storyState
                            Nothing -> toggleEpic epicState
                _ -> logError "Unknown ev" ["ev" .= ev]
            _ -> pure ()
