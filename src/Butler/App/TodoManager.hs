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

newtype TaskDesc = TaskDesc Text
    deriving newtype (Serialise, ToHtml)

newtype TaskId = TaskId Natural
    deriving newtype (Serialise, Enum, Eq, Ord, Num, Real, Integral)
data TaskSelected = TaskSelected | TaskNotSelected deriving (Generic, Serialise)

newtype TaskIndex = TaskIndex Natural
    deriving newtype (Num, Serialise)

data TaskPrio = High | Medium | Low deriving (Generic, Serialise, Show)

data TodoTask = TodoTask TaskId TaskSelected TaskDesc TaskPrio
    deriving (Generic, Serialise)

data TodoManager = TodoManager TaskIndex [TodoTask]
    deriving (Generic, Serialise)

textToPrio :: Text -> TaskPrio
textToPrio = \case
    "Low" -> Low
    "Medium" -> Medium
    "High" -> High
    _ -> error "Unable to handle taskPrio value"

buttonClass :: Attribute
buttonClass = class_ "border m-2 p-1 cursor-pointer bg-indigo-100 border-black rounded"

disabledButtonClass :: Attribute
disabledButtonClass = class_ "border m-2 p-1 bg-gray-100 border-black rounded"

button :: AppID -> TriggerName -> Text -> HtmlT STM ()
button appID triggerName displayText =
    withEvent appID triggerName [] $ do
        div_ [buttonClass] $ toHtml displayText

disabledButton :: Text -> HtmlT STM ()
disabledButton displayText =
    div_ [disabledButtonClass] $ toHtml displayText

appSumitButton :: Text -> HtmlT STM ()
appSumitButton displayText =
    button_
        [ type_ "submit"
        , buttonClass
        ]
        $ toHtml displayText

appUI :: AppContext -> MemoryVar TodoManager -> HtmlT STM ()
appUI ctx appStateM = do
    todoManager <- lift $ readMemoryVar appStateM
    div_ [id_ "MainDiv", class_ "flex flex-col"] $ do
        -- Form
        div_ [class_ "flex flex-row justify-around m-2"] $ do
            withEvent ctx.wid "add-item" [] $ do
                form_ [class_ "w-full"] $ do
                    div_ [class_ "flex flex-row flex-wrap justify-around gap-1"] $ do
                        div_ [class_ "flex flex-wrap"] $ do
                            label_ [class_ "block font-semibold m-1", for_ "taskDesc"] "Task Desc"
                            input_
                                [ id_ "taskDesc"
                                , type_ "text"
                                , name_ "taskDesc"
                                , value_ ""
                                , class_ "h-8 ml-1 text-center border border-slate-300 rounded-md focus:border-slate-400"
                                ]
                        div_ [class_ "flex flex-wrap"] $ do
                            label_ [class_ "block font-semibold m-1", for_ "taskPrio"] "Task Prio"
                            select_ [class_ "text-sm rounded", id_ "taskPrio", name_ "taskPrio"] $ do
                                option_ [value_ "High"] "High"
                                option_ [value_ "Medium"] "Medium"
                                option_ [value_ "Low"] "Low"
                    div_ [class_ "flex justify-around"] $ do
                        appSumitButton "Add Task"
                        if countSelectedTasks todoManager > 0
                            then button ctx.wid "del-item" "Del Tasks(s)"
                            else disabledButton "Del Tasks(s)"
        -- Items display
        div_ [] $ do
            showItems ctx.wid appStateM

showItems :: AppID -> MemoryVar TodoManager -> HtmlT STM ()
showItems appID appStateM = do
    (TodoManager _ todoTasks) <- lift $ readMemoryVar appStateM
    div_ [class_ "flex flex-col m-2 gap-1"] $ do
        forM_ todoTasks $ \(TodoTask taskId taskSelected taskDesc taskPrio) -> do
            div_ [class_ $ "flex flex-row align-middle gap-2 " <> taskBg taskPrio] $ do
                withEvent appID "checkbox-click" [("taskID", Number $ fromInteger $ toInteger taskId)] $ do
                    input_
                        ( [type_ "checkbox", class_ "mt-1"]
                            <> case taskSelected of
                                TaskSelected -> [checked_]
                                TaskNotSelected -> mempty
                        )

                div_ [class_ "w-16"] $ toHtml $ show taskPrio
                div_ [] $ toHtml taskDesc
  where
    taskBg :: TaskPrio -> Text
    taskBg = \case
        High -> "bg-red-100"
        Medium -> "bg-blue-100"
        Low -> "bg-green-100"

addTask :: TodoManager -> Text -> TaskPrio -> TodoManager
addTask (TodoManager (TaskIndex i) todoTasks) content taskPrio =
    let newId = i + 1
        task = newTask content taskPrio (TaskId newId)
     in TodoManager (TaskIndex newId) (task : todoTasks)

isTaskSelected :: TodoTask -> Bool
isTaskSelected (TodoTask _ taskSelected _ _) = case taskSelected of
    TaskSelected -> True
    TaskNotSelected -> False

delSelectedTasks :: TodoManager -> TodoManager
delSelectedTasks (TodoManager taskIndex todoTasks) =
    TodoManager taskIndex $ filter (not . isTaskSelected) todoTasks

countSelectedTasks :: TodoManager -> Int
countSelectedTasks (TodoManager _ todoTasks) =
    length $ filter isTaskSelected todoTasks

selectTask :: TodoTask -> TodoTask
selectTask (TodoTask taskId taskSelected taskDesc taskPrio) =
    let newSelectState = case taskSelected of
            TaskSelected -> TaskNotSelected
            TaskNotSelected -> TaskSelected
     in TodoTask taskId newSelectState taskDesc taskPrio

newTask :: Text -> TaskPrio -> TaskId -> TodoTask
newTask content taskPrio taskId = TodoTask taskId TaskNotSelected (TaskDesc content) taskPrio

setSelectedTask :: TodoManager -> TaskId -> TodoManager
setSelectedTask (TodoManager taskIndex todoTasks) taskId =
    TodoManager taskIndex $
        map
            ( \task@(TodoTask tId _ _ _) ->
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
                        case (ev.body ^? key "taskDesc" . _JSON, ev.body ^? key "taskPrio" . _JSON) of
                            (Just "", _) -> pure ()
                            (Just taskDesc, Just @Text taskPrio) -> do
                                atomically $ modifyMemoryVar appStateM $ \tm -> do
                                    addTask tm taskDesc (textToPrio taskPrio)
                            _ -> pure ()
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
