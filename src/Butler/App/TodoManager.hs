module Butler.App.TodoManager (todoManagerApp) where

import Butler
import Butler.App (withEvent)
import Butler.Core.Dynamic
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

data EditingTask = NoEditingTask | EditingTask TodoTask deriving (Generic, Serialise)

data TodoManager = TodoManager TaskIndex EditingTask [TodoTask]
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

appUI :: AppContext -> MemoryVar TodoManager -> HtmlT STM ()
appUI ctx appStateM = do
    div_ [id_ "MainDiv", class_ "flex flex-col"] $ do
        -- Form
        div_ [class_ "flex flex-row justify-around m-2"] $ do
            inputForm ctx.wid appStateM
        -- Items display
        div_ [] $ do
            showItems ctx.wid appStateM

inputForm :: AppID -> MemoryVar TodoManager -> HtmlT STM ()
inputForm appID appStateM = do
    (TodoManager _ editingTask _) <- lift $ readMemoryVar appStateM
    withEvent' editingTask $ do
        form_ [class_ "w-full"] $ do
            div_ [class_ "flex flex-row flex-wrap justify-around gap-1"] $ do
                formInputs appStateM
            div_ [class_ "flex justify-around"] $ do
                case editingTask of
                    EditingTask _ -> submitButton "Update Task"
                    NoEditingTask -> submitButton "Add Task"
                editTasksButton appID appStateM
                delTasksButton appID appStateM
  where
    withEvent' editingTask = case editingTask of
        NoEditingTask -> withEvent appID "add-item" []
        EditingTask (TodoTask taskId _ _ _) ->
            withEvent appID "edited-item" [("taskID", Number $ fromInteger $ toInteger taskId)]

formInputs :: MemoryVar TodoManager -> HtmlT STM ()
formInputs appStateM = do
    (TodoManager _ editingTask _) <- lift $ readMemoryVar appStateM
    div_ [class_ "flex flex-wrap"] $ do
        label_ [class_ "block font-semibold m-1", for_ "taskDesc"] "Task Desc"
        input_
            [ id_ "taskDesc"
            , type_ "text"
            , name_ "taskDesc"
            , value_ $ case editingTask of
                NoEditingTask -> ""
                EditingTask (TodoTask _ _ (TaskDesc taskDesc) _) -> taskDesc
            , class_ "h-8 ml-1 text-center border border-slate-300 rounded-md focus:border-slate-400"
            ]
    div_ [class_ "flex flex-wrap"] $ do
        label_ [class_ "block font-semibold m-1", for_ "taskPrio"] "Task Prio"
        select_ [class_ "text-sm rounded", id_ "taskPrio", name_ "taskPrio"] $ do
            option_ (optionAttributes editingTask "Low") "Low"
            option_ (optionAttributes editingTask "Medium") "Medium"
            option_ (optionAttributes editingTask "High") "High"
  where
    optionAttributes :: EditingTask -> Text -> [Attribute]
    optionAttributes (EditingTask (TodoTask _ _ _ taskPrio)) value =
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
    todoManager@(TodoManager _ _ todoTasks) <- lift $ readMemoryVar appStateM
    div_ [class_ "flex flex-col m-2 gap-1"] $ do
        forM_ todoTasks $ \(TodoTask taskId taskSelected taskDesc taskPrio) -> do
            div_
                [ class_ $
                    "flex flex-row align-middle gap-2 "
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
                    div_ [] $ toHtml taskDesc
  where
    taskBg :: TaskPrio -> Text
    taskBg = \case
        High -> "bg-red-100"
        Medium -> "bg-blue-100"
        Low -> "bg-green-100"

addTask :: TodoManager -> Text -> TaskPrio -> TodoManager
addTask (TodoManager (TaskIndex i) _ todoTasks) content taskPrio =
    let newId = i + 1
        task = newTask content taskPrio (TaskId newId)
     in TodoManager (TaskIndex newId) NoEditingTask (task : todoTasks)

updateTask :: TodoManager -> TaskId -> TaskDesc -> TaskPrio -> TodoManager
updateTask (TodoManager taskIndex _ todoTasks) taskId taskDesc taskPrio =
    TodoManager taskIndex NoEditingTask $
        map
            ( \task@(TodoTask tId _ _ _) ->
                if tId == taskId
                    then TodoTask taskId TaskNotSelected taskDesc taskPrio
                    else task
            )
            todoTasks

isTaskSelected :: TodoTask -> Bool
isTaskSelected (TodoTask _ taskSelected _ _) = case taskSelected of
    TaskSelected -> True
    TaskNotSelected -> False

isTaskEdited :: TodoManager -> TaskId -> Bool
isTaskEdited (TodoManager _ (EditingTask (TodoTask tId _ _ _)) _) taskId = tId == taskId
isTaskEdited _ _ = False

delSelectedTasks :: TodoManager -> TodoManager
delSelectedTasks (TodoManager taskIndex _ todoTasks) =
    TodoManager taskIndex NoEditingTask $ filter (not . isTaskSelected) todoTasks

getFirstSelectedTask :: TodoManager -> Maybe TodoTask
getFirstSelectedTask (TodoManager _ _ todoTasks) =
    case filter isTaskSelected todoTasks of
        (x : _) -> Just x
        _ -> Nothing

countSelectedTasks :: TodoManager -> Int
countSelectedTasks (TodoManager _ _ todoTasks) =
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
setSelectedTask (TodoManager taskIndex editingTask todoTasks) taskId =
    TodoManager taskIndex editingTask $
        map
            ( \task@(TodoTask tId _ _ _) ->
                if tId == taskId
                    then selectTask task
                    else task
            )
            todoTasks

setEditingTask :: TodoManager -> TodoTask -> TodoManager
setEditingTask (TodoManager taskIndex _ todoTasks) taskToEdit =
    TodoManager taskIndex (EditingTask taskToEdit) todoTasks

unSetEditingTask :: TodoManager -> TodoManager
unSetEditingTask (TodoManager taskIndex _ todoTasks) =
    TodoManager taskIndex NoEditingTask todoTasks

startTodoManager :: AppContext -> ProcessIO ()
startTodoManager ctx = do
    logInfo "TodoManager started!" []
    let appState = TodoManager 0 NoEditingTask []
        memAddr = "todo-manager-" <> showT ctx.wid <> ".bin"
    appStateM <- getSharedDynamic ctx.shared.dynamics "todo-manager" (snd <$> newProcessMemory (from memAddr) (pure appState))
    let mountUI = with div_ [wid_ ctx.wid "w"] $ appUI ctx appStateM

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
                    "edit-item" -> do
                        logInfo "Editing item" ["ev" .= ev]
                        atomically $ modifyMemoryVar appStateM $ \tm -> do
                            case getFirstSelectedTask tm of
                                Just task@(TodoTask taskID _ _ _) -> setEditingTask (setSelectedTask tm taskID) task
                                Nothing -> tm
                    "edited-item" -> do
                        logInfo "Edited item" ["ev" .= ev]
                        case (ev.body ^? key "taskID" . _JSON, ev.body ^? key "taskDesc" . _JSON, ev.body ^? key "taskPrio" . _JSON) of
                            (_, Just "", _) -> pure ()
                            (Just taskID, Just taskDesc, Just taskPrio) -> do
                                atomically $ modifyMemoryVar appStateM $ \tm -> do
                                    updateTask (unSetEditingTask tm) (TaskId taskID) (TaskDesc taskDesc) (textToPrio taskPrio)
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
