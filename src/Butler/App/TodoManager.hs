module Butler.App.TodoManager (todoManagerApp) where

import Butler
import Butler.App (withEvent)
import Butler.Core.Dynamic (getSharedDynamic)
import Data.Aeson (Value (Number))
import Data.List (sortBy)
import Data.Time (defaultTimeLocale, formatTime, parseTimeM)
import XStatic.Remixicon qualified as XStatic

todoManagerApp :: App
todoManagerApp =
    (defaultApp "TodoManager" startTodoManager)
        { tags = fromList ["Utility"]
        , description = "Manage your tasks"
        , xfiles = [XStatic.remixiconCss]
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
    deriving (Generic, Serialise, Eq, Ord, Show)

data TaskDueDate = NoDueDate | DueDate UTCTime
    deriving (Generic, Serialise, Eq)

instance Ord TaskDueDate where
    compare a b = case (a, b) of
        (NoDueDate, DueDate _) -> GT
        (DueDate _, NoDueDate) -> LT
        (DueDate a', DueDate b') -> compare a' b'
        (NoDueDate, NoDueDate) -> EQ

data TodoTask = TodoTask
    { taskId :: TaskId
    , taskSelected :: TaskSelected
    , taskDesc :: TaskDesc
    , taskPrio :: TaskPrio
    , taskDueDate :: TaskDueDate
    }
    deriving (Generic, Serialise)

data EditingTask
    = NoEditingTask
    | EditingTask TodoTask
    deriving (Generic, Serialise)

data TodoSettings = TodoSettings
    { showColumnPrio :: Bool
    , showColumnDueDate :: Bool
    }
    deriving (Generic, Serialise)

data TodoManager = TodoManager
    { todoTaskIndex :: TaskIndex
    , todoEditingTask :: EditingTask
    , todoTasks :: [TodoTask]
    , todoSettings :: TodoSettings
    , todoSettingsShow :: Bool
    }
    deriving (Generic, Serialise)

newTodoManager :: TodoManager
newTodoManager = TodoManager 0 NoEditingTask [] (TodoSettings True True) False

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
dueDateToUIDate (DueDate utcdate) = from $ formatTime defaultTimeLocale defaultDateFormat utcdate
dueDateToUIDate NoDueDate = ""

appUI :: AppContext -> MemoryVar TodoManager -> HtmlT STM ()
appUI ctx appStateM = do
    TodoManager{todoSettingsShow} <- lift $ readMemoryVar appStateM
    div_ [id_ "MainDiv", class_ "flex flex-col"] $ do
        div_ [class_ "mr-1 ml-1"] $ do
            withEvent ctx.wid "show-settings" [] $
                i_ [class_ "flex justify-end ri-settings-2-line mr-1 cursor-pointer"] mempty
        if todoSettingsShow
            then div_ [class_ "m-1 p-1 border border-gray"] $ do
                settingsPanel ctx.wid appStateM
            else mempty
        -- Form
        div_ [class_ "m-1 p-1 border border-gray flex flex-row justify-around"] $ do
            inputForm ctx.wid appStateM
        -- Items display
        div_ [class_ "m-1 p-1 border border-gray"] $ do
            showItems ctx.wid appStateM

settingsPanel :: AppID -> MemoryVar TodoManager -> HtmlT STM ()
settingsPanel appID appStateM = do
    TodoManager{todoSettings} <- lift $ readMemoryVar appStateM
    let s1 = "setting-show-column-prio"
        s2 = "setting-show-column-dueDate"
    div_ [class_ "flex flex-row flex-wrap justify-around"] $ do
        checkbox s1 "Show column 'Prio'" (TriggerName s1) todoSettings.showColumnPrio
        checkbox s2 "Show column 'DueDate'" (TriggerName s2) todoSettings.showColumnDueDate
  where
    checkbox :: Text -> Text -> TriggerName -> Bool -> HtmlT STM ()
    checkbox cid label triggerName checked = do
        div_ $ do
            withEvent appID triggerName [] $ do
                input_ $ [type_ "checkbox", id_ cid, name_ cid] <> if checked then [checked_] else mempty
                label_ [class_ "ml-1", for_ cid] $ toHtml label

inputForm :: AppID -> MemoryVar TodoManager -> HtmlT STM ()
inputForm appID appStateM = do
    TodoManager{todoEditingTask} <- lift $ readMemoryVar appStateM
    withEvent' todoEditingTask $ do
        form_ [class_ "w-full"] $ do
            div_ [class_ "flex flex-row flex-wrap justify-around gap-3"] $ do
                formInputs appStateM
            div_ [class_ "flex flex-wrap justify-around"] $ do
                case todoEditingTask of
                    EditingTask _ -> submitButton "Update Task"
                    NoEditingTask -> submitButton "Add Task"
                editTasksButton appID appStateM
  where
    withEvent' editingTask = case editingTask of
        NoEditingTask -> withEvent appID "add-item" []
        EditingTask task ->
            withEvent appID "edited-item" [("taskID", Number $ fromIntegral task.taskId)]

formInputs :: MemoryVar TodoManager -> HtmlT STM ()
formInputs appStateM = do
    TodoManager{todoEditingTask} <- lift $ readMemoryVar appStateM
    div_ [class_ "flex flex-col grow"] $ do
        label_ [class_ "block m-1", for_ "taskDesc"] "Description"
        input_
            [ id_ "taskDesc"
            , type_ "text"
            , name_ "taskDesc"
            , value_ $ case todoEditingTask of
                NoEditingTask -> ""
                EditingTask (TodoTask _ _ (TaskDesc taskDesc) _ _) -> taskDesc
            , class_ "h-8 ml-1 border border-slate-300 rounded-md focus:border-slate-400"
            ]
    div_ [class_ "flex flex-col w-32"] $ do
        label_ [class_ "block", for_ "taskPrio"] "Priority"
        select_ [class_ "text-sm rounded", id_ "taskPrio", name_ "taskPrio"] $ do
            option_ (optionAttributes todoEditingTask "Medium") "Medium"
            option_ (optionAttributes todoEditingTask "High") "High"
            option_ (optionAttributes todoEditingTask "Low") "Low"
    div_ [class_ "flex flex-col w-48"] $ do
        label_ [class_ "block", for_ "taskDueDate"] "Due date"
        input_
            [ id_ "taskDueDesc"
            , type_ "date"
            , name_ "taskDueDate"
            , value_ $ dueDateToUIDate $ case todoEditingTask of
                NoEditingTask -> NoDueDate
                EditingTask (TodoTask{taskDueDate}) -> taskDueDate
            ]
  where
    optionAttributes :: EditingTask -> Text -> [Attribute]
    optionAttributes (EditingTask (TodoTask{taskPrio})) value =
        if show taskPrio == from value
            then [value_ value, selected_ ""]
            else [value_ value]
    optionAttributes NoEditingTask value = [value_ value]

editTasksButton :: AppID -> MemoryVar TodoManager -> HtmlT STM ()
editTasksButton appID appStateM = do
    todoManager <- lift $ readMemoryVar appStateM
    if countSelectedTasks todoManager == 1
        then button appID "edit-item" "Edit Task"
        else disabledButton "Edit Task"

showItems :: AppID -> MemoryVar TodoManager -> HtmlT STM ()
showItems appID appStateM = do
    todoManager <- lift $ readMemoryVar appStateM
    div_ [class_ "flex flex-col m-2 gap-1"] $ do
        div_ [class_ "flex flex-row flex-wrap align-middle gap-2 "] $ do
            withEvent appID "del-item" [] $
                i_
                    [ class_
                        if isTasksSelected todoManager
                            then "cursor-pointer ri-delete-bin-5-line"
                            else "ri-delete-bin-line"
                    ]
                    mempty
            if todoManager.todoSettings.showColumnPrio
                then div_ [class_ "w-16 flex flex-row"] "Prio"
                else mempty
            if todoManager.todoSettings.showColumnDueDate
                then div_ [class_ "w-32"] "Due date"
                else mempty
            div_ [] "Description"
        forM_ (sortCombined todoManager.todoTasks) $ \(TodoTask{..}) -> do
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

                    if todoManager.todoSettings.showColumnPrio
                        then div_ [class_ "w-16"] $ toHtml $ show taskPrio
                        else mempty
                    if todoManager.todoSettings.showColumnDueDate
                        then div_ [class_ "w-32"] $ toHtml $ dueDateToUIDate taskDueDate
                        else mempty
                    div_ [] $ toHtml taskDesc
  where
    taskBg :: TaskPrio -> Text
    taskBg = \case
        High -> "bg-red-100"
        Medium -> "bg-green-100"
        Low -> "bg-gray-100"
    sortCombined :: [TodoTask] -> [TodoTask]
    sortCombined tasks =
        let compareTask a b =
                let c = compare a.taskPrio b.taskPrio
                 in if c == EQ then compare a.taskDueDate b.taskDueDate else c
         in sortBy compareTask tasks

addTask :: TodoManager -> Text -> TaskPrio -> TaskDueDate -> TodoManager
addTask (TodoManager (TaskIndex i) _ todoTasks todoSettings settingsShow) content taskPrio taskDueDate =
    let newId = i + 1
        task = newTask content taskPrio (TaskId newId) taskDueDate
     in TodoManager (TaskIndex newId) NoEditingTask (task : todoTasks) todoSettings settingsShow

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
isTaskEdited tm taskId' = case tm.todoEditingTask of
    EditingTask task -> task.taskId == taskId'
    NoEditingTask -> False

getEditedTask :: TodoManager -> Maybe TaskId
getEditedTask tm = case tm.todoEditingTask of
    EditingTask task -> Just task.taskId
    NoEditingTask -> Nothing

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

isTasksSelected :: TodoManager -> Bool
isTasksSelected tm = countSelectedTasks tm > 0

isTaskExists :: TodoManager -> TaskId -> Bool
isTaskExists (TodoManager{todoTasks}) taskId' =
    any (\TodoTask{taskId} -> taskId == taskId') todoTasks

selectTask :: TodoTask -> TodoTask
selectTask task@(TodoTask{taskSelected}) =
    let newSelectState = case taskSelected of
            TaskSelected -> TaskNotSelected
            TaskNotSelected -> TaskSelected
     in task{taskSelected = newSelectState}

newTask :: Text -> TaskPrio -> TaskId -> TaskDueDate -> TodoTask
newTask content taskPrio taskId = TodoTask taskId TaskNotSelected (TaskDesc content) taskPrio

setSelectedTask :: TaskId -> TodoManager -> TodoManager
setSelectedTask taskId' =
    #todoTasks
        %~ map
            ( \task@(TodoTask{taskId}) ->
                if taskId == taskId'
                    then selectTask task
                    else task
            )

setEditingTask :: TodoManager -> TodoTask -> TodoManager
setEditingTask todoManager taskToEdit =
    todoManager{todoEditingTask = EditingTask taskToEdit}

unSetEditingTask :: TodoManager -> TodoManager
unSetEditingTask todoManager =
    todoManager{todoEditingTask = NoEditingTask}

startTodoManager :: AppContext -> ProcessIO ()
startTodoManager ctx = do
    logInfo "TodoManager started!" []
    let memAddr = "todo-manager-" <> showT ctx.wid <> ".bin"
    appStateM <-
        getSharedDynamic
            ctx.shared.dynamics
            "todo-manager"
            ( snd <$> newProcessMemory (from memAddr) (pure newTodoManager)
            )
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
                        case ( ev.body ^? key "taskDesc" . _JSON
                             , ev.body ^? key "taskPrio" . _JSON
                             , ev.body ^? key "taskDueDate" . _JSON
                             ) of
                            (Just taskDesc, Just @Text taskPrio, Just taskDueDate') -> do
                                taskDueDate <- parseDueDate taskDueDate'
                                atomically $ modifyMemoryVar appStateM $ \tm -> do
                                    addTask tm taskDesc (textToPrio taskPrio) taskDueDate
                            _ -> pure ()
                    "del-item" -> do
                        logInfo "Removing item" ["ev" .= ev]
                        atomically $ modifyMemoryVar appStateM $ \tm -> do
                            let new = delSelectedTasks tm
                                editedTaskM = getEditedTask tm
                            case editedTaskM of
                                Just taskId ->
                                    if isTaskExists new taskId
                                        then new
                                        else unSetEditingTask new
                                Nothing -> new
                    "edit-item" -> do
                        logInfo "Editing item" ["ev" .= ev]
                        atomically $ modifyMemoryVar appStateM $ \tm -> do
                            case getFirstSelectedTask tm of
                                Just task@(TodoTask{..}) -> setEditingTask (setSelectedTask taskId tm) task
                                Nothing -> tm
                    "edited-item" -> do
                        logInfo "Edited item" ["ev" .= ev]
                        case ( ev.body ^? key "taskID" . _JSON
                             , ev.body ^? key "taskDesc" . _JSON
                             , ev.body ^? key "taskPrio" . _JSON
                             , ev.body ^? key "taskDueDate" . _JSON
                             ) of
                            (Just taskID, Just taskDesc, Just taskPrio, Just taskDueDate') -> do
                                taskDueDate <- parseDueDate taskDueDate'
                                atomically $ modifyMemoryVar appStateM $ \tm -> do
                                    updateTask (unSetEditingTask tm) (TaskId taskID) (TaskDesc taskDesc) (textToPrio taskPrio) taskDueDate
                            _ -> pure ()
                    "checkbox-click" -> do
                        logInfo "Selected item" ["ev" .= ev]
                        case ev.body ^? key "taskID" . _Integer of
                            Just taskId -> do
                                atomically $ modifyMemoryVar appStateM $ \tm -> do
                                    setSelectedTask (TaskId $ fromInteger $ toInteger taskId) tm
                            Nothing -> pure ()
                    "show-settings" -> do
                        logInfo "Show settings" ["ev" .= ev]
                        atomically $ modifyMemoryVar appStateM $ \tm -> do
                            tm{todoSettingsShow = not tm.todoSettingsShow}
                    "setting-show-column-prio" -> do
                        logInfo "Show settings" ["ev" .= ev]
                        atomically $ modifyMemoryVar appStateM $ \tm -> do
                            tm
                                { todoSettings =
                                    tm.todoSettings
                                        { showColumnPrio = not tm.todoSettings.showColumnPrio
                                        }
                                }
                    "setting-show-column-dueDate" -> do
                        logInfo "Show settings" ["ev" .= ev]
                        atomically $ modifyMemoryVar appStateM $ \tm -> do
                            tm
                                { todoSettings =
                                    tm.todoSettings
                                        { showColumnDueDate = not tm.todoSettings.showColumnDueDate
                                        }
                                }
                    _ -> logError "Unknown trigger" ["ev" .= ev]
            ev -> logError "Unknown ev" ["ev" .= ev]

parseDueDate :: String -> ProcessIO TaskDueDate
parseDueDate taskDueDate = do
    res <- runExceptT $ do
        if null taskDueDate
            then pure NoDueDate
            else DueDate <$> parseTimeM False defaultTimeLocale defaultDateFormat taskDueDate
    pure $ case res of
        Left _ -> NoDueDate
        Right d -> d
