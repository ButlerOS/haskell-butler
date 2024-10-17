-- no-orphans for md2jira serialise instances
{-# OPTIONS_GHC -Wno-orphans #-}

module Butler.App.MD2Jira (md2jiraApp) where

import Butler
import Butler.App
import Butler.App.JiraClient (JiraSetting (..), getJiraSetting, setScore)
import Butler.App.PokerPlanner (pokerPlannerApp)
import Butler.Core (writePipe)
import Control.OperationalTransformation.Server (Revision)
import Data.List (foldl', mapAccumL, sortBy, sortOn)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.UnixTime (UnixTime (..), getUnixTime)
import Jira (IssueData (..), JiraID, Transition, jiraUrl)
import MD2Jira
import OT qualified
import Text.Pandoc.Definition qualified as P
import Text.Pandoc.Shared qualified as P (stringify)

md2jiraApp :: App
md2jiraApp = defaultApp "md2jira" startMd2Jira

data AppState = AppState
    { doc :: Text
    , rev :: Revision
    , document :: Document
    , status :: [Text]
    }
initialState :: AppState
initialState = AppState "" (-1) (Document [] []) ["Waiting for data..."]

-- | A dummy pandoc to lucid implementation
pandocBlockToHtml :: P.Block -> HtmlT STM ()
pandocBlockToHtml = \case
    P.Plain xs -> span_ $ goInline xs
    P.Para xs -> p_ $ goInline xs
    P.CodeBlock _ txt -> with pre_ [class_ "bg-gray-800 text-white px-2 py-1"] (toHtml txt)
    P.OrderedList _ xs -> ul_ $ traverse_ (li_ . mapM_ pandocBlockToHtml) xs
    P.BulletList xs -> ul_ $ traverse_ (li_ . mapM_ pandocBlockToHtml) xs
    P.BlockQuote xs -> traverse_ pandocBlockToHtml xs
    x -> toHtml $ P.stringify x
  where
    goInline :: [P.Inline] -> HtmlT STM ()
    goInline = traverse_ pandocInlineToHtml

    pandocInlineToHtml :: P.Inline -> HtmlT STM ()
    pandocInlineToHtml = \case
        P.SoftBreak -> br_ []
        P.Strong xs -> with span_ [class_ "font-semibold"] $ goInline xs
        P.Emph xs -> with span_ [class_ "italic"] $ goInline xs
        P.Link _attr inline (url, _title) -> with a_ [href_ url, target_ "_blank"] (goInline inline)
        P.Str s -> toHtmlWithLinks s
        x -> toHtml $ P.stringify x

getStories :: [Epic] -> [Story]
getStories = concatMap goEpic
  where
    goEpic epic = epic.stories

_getTasks :: [Epic] -> [(Story, Task)]
_getTasks = concatMap goEpic
  where
    goEpic epic = concatMap goStory epic.stories
    goStory story = snd $ mapAccumL (goTask story) story.assigned story.tasks
    goTask story defaultAssignment task =
        let (storyAssigned, taskAssigned) = case (task.status, task.assigned) of
                -- This task is not assigned, use the story assignment
                (Open, []) -> ([], defaultAssignment)
                -- This task is marked as next, drop the story assignment
                (Open, ["n"]) -> ([], ["n"])
                -- Otherwise, keep the task and story assignment
                _ -> (defaultAssignment, task.assigned)
         in (storyAssigned, (story, Task task.status task.title taskAssigned task.description))

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

-- | Render text by replacing urls with <a> links
toHtmlWithLinks :: Text -> HtmlT STM ()
toHtmlWithLinks txt = case T.breakOn "https://" txt of
    (before, rest) -> do
        toHtml before
        unless (T.null rest) do
            let url = T.takeWhile (not . isSpace) rest
            with a_ [href_ url, target_ "_blank"] (toHtml url)
            toHtmlWithLinks $ T.drop (T.length url) rest

renderAge :: Bool -> CTime -> CTime -> HtmlT STM ()
renderAge withWarning (CTime now) (CTime updated)
    | withWarning && days < 3 = pure mempty
    | otherwise =
        with span_ [class_ $ "mr-1 text-xs flex items-center" <> warnClass, title_ "Age in days"] do
            toHtml $ show days <> "d"
  where
    warnClass = if withWarning && days > 14 then " text-red-600 font-bold" else ""
    days = toInteger (now - updated) `div` (3600 * 24)

renderVoteWarning :: HtmlT STM ()
renderVoteWarning = with span_ [class_ "font-bold float-right text-red-600", title_ "Story has no points or JID!"] "⚠"

storyCompletion :: Story -> Word
storyCompletion story = percent
  where
    go (tot, done) task = (tot + 1, done + if task.status == Closed then 1 else 0)
    (total, completed) = foldl' go (0 :: Word, 0 :: Word) story.tasks
    percent
        | total > 0 = completed * 100 `div` total
        | otherwise = 0

startMd2Jira :: AppContext -> ProcessIO ()
startMd2Jira ctx = do
    vState <- newTVarIO initialState

    vCache <- snd <$> newProcessMemory "cache" (pure mempty)
    vExpandedState <- newTVarIO mempty
    vErrors <- newTVarIO []

    -- Keep track of the Noter app to send update
    vNoterCtx <- newTVarIO Nothing

    -- TODO: handle client reload when setting changes...
    mSetting <- getJiraSetting ctx.shared

    let
        scroll :: AppID -> Text -> Text
        scroll quill target = "event.stopPropagation(); scrollEditor(" <> showT quill <> ", \"" <> target <> "\")"

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
        voteButton jid = with button_ [class_ (btnBlueClass <> " flex items-center w-12 h-6"), onclick_ startPokerScript] "vote"
          where
            startPokerScript = startAppScript pokerPlannerApp ["argv" .= object ["requestor" .= ctx.wid, "story" .= jid]]

        renderTaskStatus :: TaskStatus -> HtmlT STM ()
        renderTaskStatus status = with i_ [class_ "mr-1"] do
            case status of
                Open -> "☐"
                -- InProgress{assigned} -> toHtml assigned <> ":"
                Closed -> "☒"
        -- Render JiraID link
        renderJira jiraID =
            let addLink = case mSetting of
                    Nothing -> id
                    Just setting -> mappend [href_ (Jira.jiraUrl setting.client jiraID), target_ "blank"]
             in with a_ (addLink [class_ "cursor-pointer"]) (toHtml $ into @Text jiraID)

        -- Render task
        renderStoryTask :: CTime -> AppID -> Story -> [Task] -> HtmlT STM ()
        renderStoryTask now quill story tasks = do
            let color = case story.priority of
                    Just High -> "bg-red-100"
                    Just Low -> "bg-slate-100"
                    _ -> "bg-slate-200"
            with div_ (addJID story.mJira (addScroll quill story.mJira [class_ color])) do
                with div_ [class_ "flex"] do
                    with span_ [class_ "flex-grow line-clamp-1"] do
                        toHtml $ T.strip story.title
                    forM_ story.updated (renderAge True now)
                    when (isNothing story.mScore) $ maybe renderVoteWarning voteButton story.mJira
                    forM_ story.mJira renderJira
            traverse_ (renderTask quill) tasks

        renderTask :: AppID -> Task -> HtmlT STM ()
        renderTask quill task = with div_ [class_ "mb-1 bg-slate-150 flex", onclick_ (scroll quill task.title)] do
            div_ do
                renderTaskStatus task.status
            with div_ [class_ "flex-grow"] do
                with div_ [class_ "flex"] do
                    let assigned = filter (/= "n") task.assigned
                    unless (null assigned) $ with span_ [class_ "text-xs flex items-center mr-1"] do
                        traverse_ (with span_ [class_ "mr-1"] . toHtml) assigned
                        "> "
                    with span_ [class_ "flex-grow line-clamp-1"] do
                        toHtmlWithLinks $ T.strip task.title
                traverse_ pandocBlockToHtml task.description

        -- Render a story
        renderStory :: CTime -> AppID -> Map JiraID Bool -> (Story, Word) -> HtmlT STM ()
        renderStory now quill expandedState (story, percent) =
            with div_ (addJID story.mJira (addScroll quill story.mJira [class_ "mb-2 bg-slate-200"])) do
                with div_ [class_ "flex"] do
                    forM_ story.mJira (expandButton expandedState)
                    with span_ [class_ "flex-grow line-clamp-1"] do
                        toHtml $ T.strip story.title
                    when (percent > 0) do
                        with span_ [class_ "font-bold mr-2", title_ "completion"] do
                            toHtml $ into @Text $ show percent <> "%"
                    forM_ story.updated (renderAge False now)
                    forM_ story.mScore \score ->
                        with span_ [class_ "text-xs flex items-center mr-1", title_ "story points"] do toHtml $ showScore score
                    forM_ story.mJira renderJira
                    when (isNothing story.mScore) do
                        forM_ story.mJira voteButton

                when (isExpanded expandedState story.mJira) do
                    with div_ [class_ "ml-4"] do
                        traverse_ pandocBlockToHtml story.description

        -- Render a epic
        renderEpic :: CTime -> AppID -> Map JiraID Bool -> Epic -> HtmlT STM ()
        renderEpic now quill expandedState epic = with div_ (addJID epic.mJira [class_ "pb-2 mb-2"]) do
            with div_ (addScroll quill epic.mJira [class_ "flex font-semibold"]) do
                forM_ epic.mJira (expandButton expandedState)
                with span_ [class_ "flex-grow line-clamp-1"] do
                    toHtml $ T.strip epic.title
                forM_ epic.mJira renderJira
            when (isExpanded expandedState epic.mJira) do
                with div_ [class_ "ml-4"] do
                    traverse_ pandocBlockToHtml epic.description
                    mapM_ (renderStory now quill expandedState)
                        . sortOn snd -- Put the stories with the lowest completion at the top
                        . map (\story -> (story, storyCompletion story))
                        . sortOn (\story -> story.updated) -- Put the oldest stories at the top
                        $ epic.stories

        section (name :: HtmlT STM ()) xs =
            with h1_ [class_ "font-bold bg-slate-300 flex mt-4"] do
                with span_ [class_ "w-6 mr-2 block text-right"] $ toHtml (showT $ length xs)
                name

        isQueued t = t.status == Open && any (/= "n") t.assigned
        isNext t = t.status == Open && t.assigned == ["n"]

        getPrio story = fromMaybe Medium story.priority
        highPrioThenOldest s1 s2 = compare (getPrio s2) (getPrio s1) <> compare s1.updated s2.updated

        renderTaskLists :: CTime -> AppID -> [Story] -> HtmlT STM ()
        renderTaskLists now quill stories = with div_ [wid_ ctx.wid "tasks"] do
            let sortStories = sortBy highPrioThenOldest
            let queued = filter (\story -> any isQueued story.tasks) stories
            unless (null queued) do
                section "Queue" queued
                forM_ (sortStories queued) \story -> do
                    renderStoryTask now quill story (filter isQueued story.tasks)
            let next = filter (\story -> any isNext story.tasks) stories
            unless (null next) do
                section "Next" next
                forM_ (sortStories next) \story -> do
                    renderStoryTask now quill story (filter isNext story.tasks)

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
            lift (readTVar vErrors) >>= \case
                [] -> pure ()
                errors -> do
                    with span_ [class_ "text-red-600 font-bold"] "Push errors"
                    with div_ [class_ "pb-6"] do
                        mapM_ (pre_ . toHtml) errors

        -- Create the UI
        mountUI now = do
            with div_ [wid_ ctx.wid "w", class_ "my-1 mx-2"] do
                state <- lift (readTVar vState)
                quill <- maybe shellAppID (.wid) <$> lift (readTVar vNoterCtx)
                renderStatus state.rev state.status
                div_ do
                    let stories = getStories state.document.epics
                    renderTaskLists now quill stories
                    section "BackLog" $ filter (\story -> any (\t -> t.status == Open) story.tasks) stories
                    expandedState <- lift (readTVar vExpandedState)
                    mapM_ (renderEpic now quill expandedState) state.document.epics

        expandJID now quill jid issue = do
            expandedState <- atomically do
                m <- readTVar vExpandedState
                let newState
                        | isExpanded m (Just jid) = False
                        | otherwise = True
                    newM = Map.insert jid newState m
                writeTVar vExpandedState newM
                pure newM
            sendsHtml ctx.shared.clients $ case issue of
                Left epic -> renderEpic now quill expandedState epic
                Right story -> renderStory now quill expandedState (story, storyCompletion story)

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
            case OT.updateDoc state.doc (printer (state.document{epics = newEpics})) of
                Nothing -> sendsHtml ctx.shared.clients $ renderStatus state.rev state.status
                Just ops -> do
                    mvReply <- newEmptyTMVarIO
                    let msg = toDyn (state.rev, ops)
                    writePipe noterCtx.pipe (AppSync (SyncEvent "update-doc" msg mvReply))

    forever do
        aev <- atomically (readPipe ctx.pipe)
        UnixTime now _ <- liftIO getUnixTime
        case aev of
            AppDisplay (UserJoined client) -> atomically $ sendHtml client $ mountUI now
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
                        let newState = case parse doc of
                                Left err -> prevState{Butler.App.MD2Jira.status = [into err]}
                                Right document -> prevState{rev, doc, document, status = []}
                        writeTVar vState newState
                        -- expand the epic by default
                        let toggleEpic im epic = maybe im (\jid -> Map.insertWith (\_n p -> p) jid True im) epic.mJira
                        modifyTVar' vExpandedState \im -> foldl' toggleEpic im newState.document.epics
                    -- update the UI
                    sendsHtml ctx.shared.clients $ mountUI now
                _ -> logError "Bad sync" ["action" .= es.name]
            AppTrigger ev -> case ev.trigger of
                "sync" ->
                    -- Push the document to JIRA
                    withNoter \noterCtx -> withData \state -> do
                        case mSetting of
                            Nothing -> pure ()
                            Just setting -> do
                                logInfo "Syncing epics" ["epics" .= length state.document.epics]
                                sendsHtml ctx.shared.clients $ with div_ [wid_ ctx.wid "s"] do
                                    "Start syncing..."
                                cache <- atomically $ readMemoryVar vCache

                                (newEpics, newCache, errors) <- withRunInIO \run -> do
                                    let logger msg = run do
                                            logInfo msg []
                                            sendsHtml ctx.shared.clients $
                                                with div_ [wid_ ctx.wid "s", hxSwapOob_ "beforeend"] do
                                                    pre_ do toHtml msg

                                    eval logger setting.client setting.project state.document cache
                                when (newCache /= cache) do
                                    atomically $ modifyMemoryVar vCache (const newCache)
                                unless (null errors) do
                                    -- TODO: send the errors to the client
                                    logError "md2jira eval errors!" ["errors" .= errors]
                                atomically $ writeTVar vErrors errors
                                -- Generate and send text operation to noter
                                updateNoter noterCtx state newEpics.epics
                "toggle" | Just jid <- ev.body ^? key "jid" . _JSON -> withNoter \noterCtx -> withData \state ->
                    case findIssue state.document.epics jid of
                        Nothing -> logError "unknown toggle" ["jid" .= jid]
                        Just issue -> expandJID now noterCtx.wid jid issue
                "poker-result" ->
                    case (ev.body ^? key "value" . _JSON, ev.body ^? key "story" . _JSON, ev.body ^? key "wid" . _JSON) of
                        (Just score, Just jid, Just wid) -> withNoter \noterCtx -> withData \state -> do
                            closeApp ctx.shared ev.client wid
                            forM_ mSetting \setting -> do
                                sendsHtml ctx.shared.clients $
                                    with div_ [wid_ ctx.wid "s"] do
                                        pre_ $ "Submiting vote for " <> toHtml (into @Text jid) <> "..."

                                setScore setting jid score >>= \case
                                    True -> updateNoter noterCtx state $ updateScore score jid state.document.epics
                                    False -> do
                                        let msg = "Vote failed to sync on JIRA " <> into @Text jid
                                        sendsHtml ctx.shared.clients $ with div_ [wid_ ctx.wid "s"] do
                                            toHtml msg
                                        atomically $ modifyTVar' vErrors (msg :)
                        res -> logError "Unknown result" ["ev" .= ev, "res" .= res]
                _ -> logError "Unknown ev" ["ev" .= ev]
            _ -> pure ()

instance Serialise IssueData
instance Serialise JiraID
instance Serialise CacheEntry
instance Serialise Transition
