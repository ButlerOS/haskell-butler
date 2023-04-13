module Butler.App.TodoManager (todoManagerApp) where

import Butler
import Butler.App (withEvent)
import Butler.Core.Dynamic
import Data.Aeson (Value (Number))
import Data.Time (defaultTimeLocale, formatTime, parseTimeM)

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

data TaskSelected
    = TaskSelected
    | TaskNotSelected
    deriving (Generic, Serialise)

newtype TaskIndex = TaskIndex Natural
    deriving newtype (Num, Serialise)

data TaskPrio
    = High
    | Medium
    | Low
    deriving (Generic, Serialise, Show)

newtype TaskDueDate = TaskDueDate UTCTime
    deriving newtype (Serialise)

data TodoTask = TodoTask
    { taskId :: TaskId
    , taskSelected :: TaskSelected
    , taskDesc :: TaskDesc
    , taskPrio :: TaskPrio
    , taskDueDate :: TaskDueDate
    }
    deriving (Generic, Serialise)

data EditingTask = NoEditingTask | EditingTask TodoTask deriving (Generic, Serialise)

data TodoManager = TodoManager
    { todoTaskIndex :: TaskIndex
    , todoEditingTask :: EditingTask
    , todoTasks :: [TodoTask]
    }
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

submitButton :: Text -> HtmlT STM ()
submitButton displayText =
    button_
        [ type_ "submit"
        , buttonClass
        ]
        $ toHtml displayText

defaultDateFormat :: String
defaultDateFormat = "%Y-%m-%d"

dueDateToUIDate :: TaskDueDate -> Text
dueDateToUIDate (TaskDueDate utcdate) = from $ formatTime defaultTimeLocale defaultDateFormat utcdate

appUI :: AppContext -> MemoryVar TodoManager -> TaskDueDate -> HtmlT STM ()
appUI ctx appStateM taskDueDate = do
    div_ [id_ "MainDiv", class_ "flex flex-col"] $ do
        -- Form
        div_ [class_ "flex flex-row justify-around m-2"] $ do
            inputForm ctx.wid appStateM taskDueDate
        -- Items display
        div_ [] $ do
            showItems ctx.wid appStateM

inputForm :: AppID -> MemoryVar TodoManager -> TaskDueDate -> HtmlT STM ()
inputForm appID appStateM taskDueDate = do
    TodoManager{todoEditingTask} <- lift $ readMemoryVar appStateM
    withEvent' todoEditingTask $ do
        form_ [class_ "w-full"] $ do
            div_ [class_ "flex flex-row flex-wrap justify-around gap-1"] $ do
                formInputs appStateM taskDueDate
            div_ [class_ "flex justify-around"] $ do
                case todoEditingTask of
                    EditingTask _ -> submitButton "Update Task"
                    NoEditingTask -> submitButton "Add Task"
                editTasksButton appID appStateM
                delTasksButton appID appStateM
  where
    withEvent' editingTask = case editingTask of
        NoEditingTask -> withEvent appID "add-item" []
        EditingTask task ->
            withEvent appID "edited-item" [("taskID", Number $ fromIntegral task.taskId)]

formInputs :: MemoryVar TodoManager -> TaskDueDate -> HtmlT STM ()
formInputs appStateM defaultTaskDueDate = do
    TodoManager{todoEditingTask} <- lift $ readMemoryVar appStateM
    div_ [class_ "flex flex-wrap"] $ do
        label_ [class_ "block font-semibold m-1", for_ "taskDesc"] "Task Desc"
        input_
            [ id_ "taskDesc"
            , type_ "text"
            , name_ "taskDesc"
            , value_ $ case todoEditingTask of
                NoEditingTask -> ""
                EditingTask (TodoTask _ _ (TaskDesc taskDesc) _ _) -> taskDesc
            , class_ "h-8 ml-1 text-center border border-slate-300 rounded-md focus:border-slate-400"
            ]
    div_ [class_ "flex flex-wrap"] $ do
        label_ [class_ "block font-semibold m-1", for_ "taskPrio"] "Task Prio"
        select_ [class_ "text-sm rounded", id_ "taskPrio", name_ "taskPrio"] $ do
            option_ (optionAttributes todoEditingTask "Low") "Low"
            option_ (optionAttributes todoEditingTask "Medium") "Medium"
            option_ (optionAttributes todoEditingTask "High") "High"
    div_ [class_ "flex flex-wrap"] $ do
        label_ [class_ "block font-semibold m-1", for_ "taskDueDate"] "Due date"
        input_
            [ id_ "taskDueDesc"
            , type_ "date"
            , name_ "taskDueDate"
            , value_ $ dueDateToUIDate $ case todoEditingTask of
                NoEditingTask -> defaultTaskDueDate
                EditingTask (TodoTask{taskDueDate}) -> taskDueDate
            ]
  where
    optionAttributes :: EditingTask -> Text -> [Attribute]
    optionAttributes (EditingTask (TodoTask{taskPrio})) value =
        if show taskPrio == from value
            then [value_ value, selected_ ""]
            else [value_ value]
    optionAttributes NoEditingTask value = [value_ value]

delTasksButton :: AppID -> MemoryVar TodoManager -> HtmlT STM ()
delTasksButton appID appStateM = do
    todoManager <- lift $ readMemoryVar appStateM
    if countSelectedTasks todoManager > 0
        then button appID "del-item" "Del Tasks(s)"
        else disabledButton "Del Tasks(s)"

editTasksButton :: AppID -> MemoryVar TodoManager -> HtmlT STM ()
editTasksButton appID appStateM = do
    todoManager <- lift $ readMemoryVar appStateM
    if countSelectedTasks todoManager == 1
        then button appID "edit-item" "Edit Task"
        else disabledButton "Edit Task"

showItems :: AppID -> MemoryVar TodoManager -> HtmlT STM ()
showItems appID appStateM = do
    todoManager@(TodoManager{todoTasks}) <- lift $ readMemoryVar appStateM
    div_ [class_ "flex flex-col m-2 gap-1"] $ do
        forM_ todoTasks $ \(TodoTask{..}) -> do
            div_
                [ class_ $
                    "flex flex-row flex-wrap align-middle gap-2 "
                        <> taskBg taskPrio
                        <> if isTaskEdited todoManager taskId then " border-2 border-dashed border-pink-300" else mempty
                ]
                $ do
                    withEvent appID "checkbox-click" [("taskID", Number $ fromInteger $ toInteger taskId)] $ do
                        input_
                            ( [type_ "checkbox", class_ "mt-1"]
                                <> case taskSelected of
                                    TaskSelected -> [checked_]
                                    TaskNotSelected -> mempty
                            )

                    div_ [class_ "w-16"] $ toHtml $ show taskPrio
                    div_ [class_ "w-32"] $ toHtml $ dueDateToUIDate taskDueDate
                    div_ [] $ toHtml taskDesc
  where
    taskBg :: TaskPrio -> Text
    taskBg = \case
        High -> "bg-red-100"
        Medium -> "bg-blue-100"
        Low -> "bg-green-100"

addTask :: TodoManager -> Text -> TaskPrio -> TaskDueDate -> TodoManager
addTask (TodoManager (TaskIndex i) _ todoTasks) content taskPrio taskDueDate =
    let newId = i + 1
        task = newTask content taskPrio (TaskId newId) taskDueDate
     in TodoManager (TaskIndex newId) NoEditingTask (task : todoTasks)

updateTask :: TodoManager -> TaskId -> TaskDesc -> TaskPrio -> TaskDueDate -> TodoManager
updateTask todoManager@(TodoManager{todoTasks}) taskId' taskDesc taskPrio taskDueDate =
    todoManager
        { todoEditingTask = NoEditingTask
        , todoTasks =
            map
                ( \task@(TodoTask{taskId}) ->
                    if taskId == taskId'
                        then TodoTask taskId' TaskNotSelected taskDesc taskPrio taskDueDate
                        else task
                )
                todoTasks
        }

isTaskSelected :: TodoTask -> Bool
isTaskSelected (TodoTask{taskSelected}) = case taskSelected of
    TaskSelected -> True
    TaskNotSelected -> False

isTaskEdited :: TodoManager -> TaskId -> Bool
isTaskEdited (TodoManager _ (EditingTask (TodoTask{taskId})) _) taskId' = taskId == taskId'
isTaskEdited _ _ = False

delSelectedTasks :: TodoManager -> TodoManager
delSelectedTasks todoManager@(TodoManager{todoTasks}) =
    todoManager{todoTasks = filter (not . isTaskSelected) todoTasks}

getFirstSelectedTask :: TodoManager -> Maybe TodoTask
getFirstSelectedTask (TodoManager{todoTasks}) =
    case filter isTaskSelected todoTasks of
        (x : _) -> Just x
        _ -> Nothing

countSelectedTasks :: TodoManager -> Int
countSelectedTasks (TodoManager{todoTasks}) =
    length $ filter isTaskSelected todoTasks

selectTask :: TodoTask -> TodoTask
selectTask task@(TodoTask{taskSelected}) =
    let newSelectState = case taskSelected of
            TaskSelected -> TaskNotSelected
            TaskNotSelected -> TaskSelected
     in task{taskSelected = newSelectState}

newTask :: Text -> TaskPrio -> TaskId -> TaskDueDate -> TodoTask
newTask content taskPrio taskId = TodoTask taskId TaskNotSelected (TaskDesc content) taskPrio

setSelectedTask :: TodoManager -> TaskId -> TodoManager
setSelectedTask (TodoManager{..}) taskId' =
    TodoManager todoTaskIndex todoEditingTask $
        map
            ( \task@(TodoTask{taskId}) ->
                if taskId == taskId'
                    then selectTask task
                    else task
            )
            todoTasks

setEditingTask :: TodoManager -> TodoTask -> TodoManager
setEditingTask todoManager taskToEdit =
    todoManager{todoEditingTask = EditingTask taskToEdit}

unSetEditingTask :: TodoManager -> TodoManager
unSetEditingTask todoManager =
    todoManager{todoEditingTask = NoEditingTask}

startTodoManager :: AppContext -> ProcessIO ()
startTodoManager ctx = do
    logInfo "TodoManager started!" []
    let appState = TodoManager 0 NoEditingTask []
        memAddr = "todo-manager-" <> showT ctx.wid <> ".bin"
    appStateM <- getSharedDynamic ctx.shared.dynamics "todo-manager" (snd <$> newProcessMemory (from memAddr) (pure appState))
    now <- liftIO getCurrentTime
    let mountUI = with div_ [wid_ ctx.wid "w"] $ appUI ctx appStateM defaultTaskDueDate
        defaultTaskDueDate = TaskDueDate now

    spawnThread_ $ renderOnChange mountUI \newHtml -> do
        sendsHtml ctx.shared.clients newHtml

    -- Handle events
    forever do
        atomically (readPipe ctx.pipe) >>= \case
            AppDisplay (UserJoined client) -> atomically $ sendHtml client mountUI
            AppTrigger ev -> do
                case ev.trigger of
                    "add-item" -> do
                        logInfo "Adding item" ["ev" .= ev]
                        case ( ev.body ^? key "taskDesc" . _JSON
                             , ev.body ^? key "taskPrio" . _JSON
                             , ev.body ^? key "taskDueDate" . _JSON
                             ) of
                            (Just taskDesc, Just @Text taskPrio, Just taskDueDate') -> do
                                taskDueDate <- parseTimeM False defaultTimeLocale defaultDateFormat taskDueDate'
                                atomically $ modifyMemoryVar appStateM $ \tm -> do
                                    addTask tm taskDesc (textToPrio taskPrio) (TaskDueDate taskDueDate)
                            _ -> pure ()
                    "del-item" -> do
                        logInfo "Removing item" ["ev" .= ev]
                        atomically $ modifyMemoryVar appStateM $ \tm -> do
                            delSelectedTasks tm
                    "edit-item" -> do
                        logInfo "Editing item" ["ev" .= ev]
                        atomically $ modifyMemoryVar appStateM $ \tm -> do
                            case getFirstSelectedTask tm of
                                Just task@(TodoTask{..}) -> setEditingTask (setSelectedTask tm taskId) task
                                Nothing -> tm
                    "edited-item" -> do
                        logInfo "Edited item" ["ev" .= ev]
                        case ( ev.body ^? key "taskID" . _JSON
                             , ev.body ^? key "taskDesc" . _JSON
                             , ev.body ^? key "taskPrio" . _JSON
                             , ev.body ^? key "taskDueDate" . _JSON
                             ) of
                            (Just taskID, Just taskDesc, Just taskPrio, Just taskDueDate') -> do
                                taskDueDate <- parseTimeM False defaultTimeLocale defaultDateFormat taskDueDate'
                                atomically $ modifyMemoryVar appStateM $ \tm -> do
                                    updateTask (unSetEditingTask tm) (TaskId taskID) (TaskDesc taskDesc) (textToPrio taskPrio) (TaskDueDate taskDueDate)
                            _ -> pure ()
                    "checkbox-click" -> do
                        logInfo "Selected item" ["ev" .= ev]
                        case ev.body ^? key "taskID" . _Integer of
                            Just taskId -> do
                                atomically $ modifyMemoryVar appStateM $ \tm -> do
                                    setSelectedTask tm $ TaskId $ fromInteger $ toInteger taskId
                            Nothing -> pure ()
                    _ -> logError "Unknown trigger" ["ev" .= ev]
            ev -> logError "Unknown ev" ["ev" .= ev]
