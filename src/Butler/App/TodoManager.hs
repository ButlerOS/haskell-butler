module Butler.App.TodoManager (todoManagerApp) where

import Butler
import Butler.App (withEvent)
import Data.Aeson (Value (Number))

todoManagerApp :: App
todoManagerApp =
    (defaultApp "TodoManager" startTodoManager)
        { tags = fromList ["Utility"]
        , description = "Manage your tasks"
        }

newtype TaskDesc = TaskDesc Text deriving newtype (Serialise, ToHtml)
newtype TaskId = TaskId Natural deriving newtype (Serialise, Enum, Eq, Ord, Num, Real, Integral)
data TaskSelected = TaskSelected | TaskNotSelected deriving (Generic, Serialise)
newtype TaskIndex = TaskIndex Natural
    deriving newtype (Num, Serialise)

data TodoTask = TodoTask TaskId TaskSelected TaskDesc deriving (Generic, Serialise)

data TodoManager = TodoManager TaskIndex [TodoTask] deriving (Generic, Serialise)

appButtonClass :: Attribute
appButtonClass = class_ "border m-2 p-1 cursor-pointer bg-indigo-100 border-black rounded"

appButton :: AppID -> TriggerName -> Text -> HtmlT STM ()
appButton appID triggerName displayText =
    withEvent appID triggerName [] $ do
        div_ [appButtonClass] $ toHtml displayText

appSumitButton :: Text -> HtmlT STM ()
appSumitButton displayText =
    button_
        [ type_ "submit"
        , appButtonClass
        ]
        $ toHtml displayText

appUI :: AppContext -> MemoryVar TodoManager -> HtmlT STM ()
appUI ctx appStateM = do
    div_ [id_ "MainDiv", class_ "flex flex-col"] $ do
        div_ [class_ "flex flex-row justify-around"] $ do
            appButton ctx.wid "del-item" "Del Item(s)"
            withEvent ctx.wid "add-item" [] $ do
                form_ [] $ do
                    label_ [class_ "font-semibold"] "Task description"
                    input_
                        [ type_ "text"
                        , placeholder_ "Enter task description"
                        , name_ "taskDesc"
                        , value_ ""
                        , size_ "25"
                        , maxlength_ "15"
                        , class_ "h-8 ml-1 text-center border border-slate-300 rounded-md focus:border-slate-400"
                        ]
                    appSumitButton "Add Item"
        div_ [] $ do
            showItems ctx.wid appStateM

showItems :: AppID -> MemoryVar TodoManager -> HtmlT STM ()
showItems appID appStateM = do
    (TodoManager _ todoTasks) <- lift $ readMemoryVar appStateM
    div_ [class_ "flex flex-col m-2 gap-1"] $ do
        forM_ todoTasks $ \(TodoTask taskId taskSelected taskDesc) -> do
            div_ [class_ "flex flex-row gap-2"] $ do
                withEvent appID "checkbox-click" [("taskID", Number $ fromInteger $ toInteger taskId)] $ do
                    input_
                        ( [type_ "checkbox"]
                            <> case taskSelected of
                                TaskSelected -> [checked_]
                                TaskNotSelected -> mempty
                        )

                div_ [] $ toHtml taskDesc

addTask :: TodoManager -> Text -> TodoManager
addTask (TodoManager (TaskIndex i) todoTasks) content =
    let newId = i + 1
        task = newTask content (TaskId newId)
     in TodoManager (TaskIndex newId) (task : todoTasks)

delSelectedTasks :: TodoManager -> TodoManager
delSelectedTasks (TodoManager taskIndex todoTasks) =
    TodoManager taskIndex $
        filter
            ( \(TodoTask _ taskSelected _) ->
                case taskSelected of
                    TaskSelected -> False
                    TaskNotSelected -> True
            )
            todoTasks

selectTask :: TodoTask -> TodoTask
selectTask (TodoTask taskId taskSelected taskDesc) =
    let newSelectState = case taskSelected of
            TaskSelected -> TaskNotSelected
            TaskNotSelected -> TaskSelected
     in TodoTask taskId newSelectState taskDesc

newTask :: Text -> TaskId -> TodoTask
newTask content taskId = TodoTask taskId TaskNotSelected $ TaskDesc content

setSelectedTask :: TodoManager -> TaskId -> TodoManager
setSelectedTask (TodoManager taskIndex todoTasks) taskId =
    TodoManager taskIndex $
        map
            ( \task@(TodoTask tId _ _) ->
                if tId == taskId
                    then selectTask task
                    else task
            )
            todoTasks

startTodoManager :: AppContext -> ProcessIO ()
startTodoManager ctx = do
    logInfo "TodoManager started!" []
    let appState = TodoManager 0 []
        memAddr = "todo-manager-" <> showT ctx.wid <> ".bin"
    (_, appStateM) <- newProcessMemory (from memAddr) (pure appState)
    let mountUI = with div_ [wid_ ctx.wid "w"] $ appUI ctx appStateM

    -- Handle events
    forever do
        atomically (readPipe ctx.pipe) >>= \case
            AppDisplay (UserJoined client) -> atomically $ sendHtml client mountUI
            AppTrigger ev -> do
                case ev.trigger of
                    "add-item" -> do
                        logInfo "Adding item" ["ev" .= ev]
                        case ev.body ^? key "taskDesc" . _JSON of
                            Just taskDesc -> do
                                atomically $ modifyMemoryVar appStateM $ \tm -> do
                                    addTask tm taskDesc
                            Nothing -> pure ()
                    "del-item" -> do
                        logInfo "Removing item" ["ev" .= ev]
                        atomically $ modifyMemoryVar appStateM $ \tm -> do
                            delSelectedTasks tm
                    "checkbox-click" -> do
                        logInfo "Selected item" ["ev" .= ev]
                        case ev.body ^? key "taskID" . _Integer of
                            Just taskId -> do
                                atomically $ modifyMemoryVar appStateM $ \tm -> do
                                    setSelectedTask tm $ TaskId $ fromInteger $ toInteger taskId
                            Nothing -> pure ()
                    _ -> logError "Unknown trigger" ["ev" .= ev]
                sendsHtml ctx.shared.clients mountUI
            ev -> logError "Unknown ev" ["ev" .= ev]
