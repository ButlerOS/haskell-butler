module Butler.App.MD2Jira (md2jiraApp) where

import Butler
import Butler.App
import Butler.Core (writePipe)
import Control.OperationalTransformation.Server (Revision)
import Jira (IssueData (..))
import MD2Jira
import OT qualified

md2jiraApp :: App
md2jiraApp = defaultApp "md2jira" startMd2Jira

startMd2Jira :: AppContext -> ProcessIO ()
startMd2Jira ctx = do
    (vData :: TVar (Either String (Revision, Text, [Epic]))) <- newTVarIO (Left "no data...")

    let
        renderJira jiraID =
            with a_ [class_ "float-right cursor-pointer"] (toHtml $ into @Text jiraID)

        renderStory :: Story -> HtmlT STM ()
        renderStory story = with li_ [class_ "mt-2"] do
            forM_ story.mJira renderJira
            h2_ $ toHtml $ "⊝ Story: " <> story.info.summary
            with div_ [class_ "ml-2"] do
                pre_ $ toHtml $ story.info.description

        renderEpic :: Epic -> HtmlT STM ()
        renderEpic epic = with li_ [class_ "mb-4"] do
            div_ do
                forM_ epic.mJira renderJira

                with h1_ [class_ "font-semibold"] (toHtml $ "⊗ Epic: " <> epic.info.summary)
                with div_ [class_ "ml-2"] do
                    pre_ $ toHtml $ epic.info.description
                    ul_ do
                        mapM_ renderStory epic.stories

        mountUI = do
            with div_ [wid_ ctx.wid "w"] do
                lift (readTVar vData) >>= \case
                    Left err -> div_ $ toHtml $ "Oops: " <> into @Text err
                    Right (_rev, _oldDoc, xs) -> do
                        withTrigger_ "click" ctx.wid "sync" button_ [class_ ("my-1 mx-2 " <> btnBlueClass)] "SYNC"
                        with ul_ [class_ "mx-2 my-1 list-disc"] $ mapM_ renderEpic xs
                        br_ []
                        pre_ $ toHtml $ printer xs

    vNoterCtx <- newTVarIO Nothing
    forever do
        atomically (readPipe ctx.pipe) >>= \case
            AppDisplay (UserJoined client) -> atomically $ sendHtml client mountUI
            AppSync es -> case es.name of
                "start-repl" | Just (noterCtx, _) <- fromDynamic @(AppContext, [String]) es.message -> do
                    atomically $ writeTVar vNoterCtx (Just noterCtx)
                    atomically (putTMVar es.reply (toDyn ()))
                "new-doc" | Just (rev, doc) <- fromDynamic @(Revision, Text) es.message -> do
                    atomically $ writeTVar vData $ (rev,doc,) <$> parse doc
                    sendsHtml ctx.shared.clients mountUI
                _ -> logError "Bad sync" ["action" .= es.name]
            AppTrigger ev -> case ev.trigger of
                "sync" ->
                    readTVarIO vNoterCtx >>= \case
                        Nothing -> logError "No Noter Application for syncing..." []
                        Just noterCtx ->
                            readTVarIO vData >>= \case
                                Left err -> logError "Can't sync error" ["err" .= err]
                                Right (rev, oldDoc, epics) -> do
                                    mvReply <- newEmptyTMVarIO
                                    let newDoc = printer epics
                                    let ops = OT.updateDoc oldDoc newDoc
                                    let msg = toDyn (rev, ops)
                                    writePipe noterCtx.pipe (AppSync (SyncEvent "update-doc" msg mvReply))
                _ -> logError "Unknown ev" ["ev" .= ev]
            _ -> pure ()
