module Butler.App.Noter (noterApp) where

import Butler
import Butler.Display.Session
import Butler.Display.User
import Butler.Frame
import Butler.Service.FileService

import Data.Map.Strict qualified as Map
import Data.Text.Rope as Rope

noterApp :: App
noterApp =
    (defaultApp "noter" startNoterApp)
        { tags = fromList ["Utility", "Development"]
        , description = "Text editor"
        , start = startNoterApp
        , acceptFiles = Just TextContent
        }

data Editor = Editor
    { client :: DisplayClient
    , position :: Word
    }
    deriving (Generic)

data EditorAction
    = MoveCursor Word
    | Insert Text
    | Delete Int

instance FromJSON EditorAction where
    parseJSON = withObject "EditorAction" \obj -> do
        let
            moveParser =
                obj .:? "move" >>= \case
                    Just pos -> pure (MoveCursor pos)
                    Nothing -> fail "missing action"
            insertParser =
                obj .:? "insert" >>= \case
                    Just txt -> pure (Insert txt)
                    Nothing -> fail "missing action"
            deleteParser =
                obj .:? "delete" >>= \case
                    Just pos -> pure (Delete pos)
                    Nothing -> fail "missing action"
        moveParser <|> insertParser <|> deleteParser

data NoterStatus
    = NewFile
    | EditingFile Directory File

data NoterState = NoterState
    { status :: NoterStatus
    , dirty :: Bool
    , content :: Rope
    , editors :: Map Endpoint Editor
    }
    deriving (Generic)

modifyEditors :: (Map Endpoint Editor -> Map Endpoint Editor) -> NoterState -> (Map Endpoint Editor, NoterState)
modifyEditors edit state =
    let newEditors = edit state.editors
     in (newEditors, state & #editors .~ newEditors)

startNoterApp :: AppContext -> ProcessIO ()
startNoterApp ctx = do
    (currentFile, memFile) <- newProcessMemory (from $ withWID ctx.wid "noter-file") (pure mempty)
    rootDir <- getVolumeDirectory ctx.shared Nothing
    tState <-
        atomically (resolveFileLoc rootDir currentFile) >>= \case
            Just (dir, Just file) -> do
                content <- decodeUtf8 <$> readFileBS dir file
                newTVarIO $ NoterState (EditingFile dir file) False (Rope.fromText content) mempty
            _ -> newTVarIO (NoterState NewFile False mempty mempty)

    let saveBtn = withTrigger_ "click" ctx.wid "save-file" button_ [class_ ("mx-2 " <> btnGreenClass)] "Save"
        fileNameInput = withTrigger_ "" ctx.wid "new-file" (input_ []) [type_ "text", placeholder_ "File name", name_ "file"]
        editorList editors =
            with div_ [wid_ ctx.wid "editors"] do
                forM_ editors \editor -> do
                    userIcon =<< lift (readTVar editor.client.session.username)

        fileNameForm state = do
            with div_ [wid_ ctx.wid "name", class_ "flex-grow"] do
                case state.status of
                    EditingFile _ file -> do
                        when state.dirty saveBtn
                        span_ (toHtml (into @FileName file.name))
                    NewFile -> fileNameInput
    let mountUI = do
            state <- lift (readTVar tState)
            with div_ [wid_ ctx.wid "w", class_ "w-full h-full flex flex-col"] do
                with div_ [class_ "flex flex-row"] do
                    withTrigger_ "click" ctx.wid "refresh" i_ [class_ "ri-refresh-line cursor-pointer mx-1"] mempty
                    fileNameForm state
                    editorList state.editors
                with div_ [class_ "flex-grow", wid_ ctx.wid "txt-div"] do
                    with textarea_ [wid_ ctx.wid "txt", class_ "w-full h-full"] (toHtmlRaw (toText state.content))
                script_ (noterClient ctx.wid)

    let handleEditorAction client = \case
            MoveCursor pos -> do
                logDebug "cursor moved" ["client" .= client, "pos" .= pos]
                let updatePosition editor
                        | editor.client.endpoint == client.endpoint = editor & #position .~ pos
                        | otherwise = editor
                -- TODO: broadcast and render cursor to the other clients
                atomically $ modifyTVar' tState (#editors %~ fmap updatePosition)
            Insert txt -> do
                mBody <- atomically $ stateTVar tState \state ->
                    case Map.lookup client.endpoint state.editors of
                        Nothing -> (Nothing, state)
                        Just editor ->
                            let
                                -- update content
                                (before, after) = Rope.splitAt editor.position state.content
                                insertRope = Rope.fromText txt
                                newContent = mconcat [before, insertRope, after]
                                setContent = #content .~ newContent
                                -- update editor position (cursor set after the insertion)
                                newEditor = editor & #position .~ editor.position + Rope.length insertRope
                                updateEditor = #editors %~ Map.insert client.endpoint newEditor
                                newState = state & setContent . updateEditor . (#dirty .~ True)
                                dirtyChanged
                                    | state.dirty = Nothing
                                    | otherwise = Just newState
                                -- broadcast event
                                body = object ["insert" .= txt, "pos" .= editor.position]
                             in
                                (Just (dirtyChanged, body), newState)
                case mBody of
                    Just (dirtyChanged, body) -> do
                        sendsBinaryButSelf client ctx.clients (encodeMessageL ctx.wid (encodeJSON body))
                        forM_ dirtyChanged (sendsHtml ctx.clients . fileNameForm)
                    Nothing -> logError "Insert failed" ["client" .= client, "txt" .= txt]
            Delete count -> do
                -- logDebug "Deleting" ["count" .= count]
                mBody <- atomically $ stateTVar tState \state ->
                    case Map.lookup client.endpoint state.editors of
                        Nothing -> (Nothing, state)
                        Just editor ->
                            let
                                -- The absolute deletion size
                                deleteSize = fromIntegral (abs count)
                                deleteStart
                                    | -- negative count is backward delete
                                      count < 0 =
                                        editor.position - deleteSize
                                    | otherwise = editor.position
                                -- update content
                                (before, rest) = Rope.splitAt deleteStart state.content
                                (_deleted, after) = Rope.splitAt deleteSize rest
                                newContent = mconcat [before, after]
                                -- update editor position
                                updateEditor
                                    | count < 0 =
                                        let newPosition
                                                | deleteSize < editor.position = editor.position - deleteSize
                                                | otherwise = 0
                                         in Map.insert client.endpoint (editor & #position .~ newPosition)
                                    | otherwise = id
                                newState = state & (#content .~ newContent) . (#editors %~ updateEditor) . (#dirty .~ True)
                                dirtyChanged
                                    | state.dirty = Nothing
                                    | otherwise = Just newState
                                -- broadcast event
                                body = object ["delete" .= abs count, "pos" .= deleteStart]
                             in
                                (Just (dirtyChanged, body), newState)
                case mBody of
                    Just (dirtyChanged, body) -> do
                        sendsBinaryButSelf client ctx.clients (encodeMessageL ctx.wid (encodeJSON body))
                        forM_ dirtyChanged (sendsHtml ctx.clients . fileNameForm)
                    Nothing -> logError "Delete failed" ["client" .= client, "count" .= count]
    forever do
        atomically (readPipe ctx.pipe) >>= \case
            AppDisplay (UserConnected "htmx" client) -> do
                editors <- atomically $ stateTVar tState (modifyEditors $ Map.insert client.endpoint (Editor client 0))
                atomically $ sendHtml client mountUI
                sendsHtmlButSelf client ctx.clients (editorList editors)
            AppDisplay (UserDisconnected "htmx" client) -> do
                editors <- atomically $ stateTVar tState (modifyEditors $ Map.delete client.endpoint)
                sendsHtml ctx.clients (editorList editors)
            AppTrigger ev -> case ev.trigger of
                "refresh" -> atomically do
                    content <- (.content) <$> readTVar tState
                    sendBinary ev.client (encodeMessageL ctx.wid (encodeJSON $ object ["text" .= toText content]))
                "save-file" -> do
                    state <- readTVarIO tState
                    case state.status of
                        NewFile -> logError "Need a file name" ["ev" .= ev]
                        EditingFile dir file -> do
                            writeFileBS dir file (encodeUtf8 $ Rope.toText state.content)
                            let newState = state & (#dirty .~ False)
                            atomically $ writeTVar tState newState
                            sendsHtml ctx.clients (fileNameForm newState)
                "new-file" -> case ev.body ^? key "file" . _JSON of
                    Just fp -> do
                        state <- readTVarIO tState
                        mDir <- case state.status of
                            NewFile -> atomically $ newDirectory rootDir "Documents"
                            EditingFile curDir _ -> pure (Just curDir)
                        case mDir of
                            Just dir -> do
                                file <- createFile dir fp (encodeUtf8 $ Rope.toText state.content)
                                let newState = state & (#status .~ EditingFile dir file) . (#dirty .~ False)
                                atomically do
                                    writeTVar tState newState
                                    modifyMemoryVar memFile (const $ getFileLoc dir (Just file))
                                sendsHtml ctx.clients (fileNameForm newState)
                            Nothing -> logError "Documents is not a directory" []
                    Nothing -> logError "Invalid new-file" ["ev" .= ev]
                _ -> logError "Unknown ev" ["ev" .= ev]
            AppData ev -> case decodeJSON @EditorAction (from ev.buffer) of
                Just action -> handleEditorAction ev.client action
                Nothing -> logError "Unknown action" ["ev" .= ev]
            AppFile dir (Just file) -> do
                content <- decodeUtf8 <$> readFileBS dir file
                atomically do
                    modifyMemoryVar memFile (const $ getFileLoc dir (Just file))
                    let setContent = #content .~ Rope.fromText content
                    modifyTVar' tState $ (#status .~ EditingFile dir file) . (#dirty .~ False) . setContent
                sendsHtml ctx.clients mountUI
            _ -> pure ()

-- See https://developer.mozilla.org/en-US/docs/Web/API/HTMLTextAreaElement
noterClient :: WinID -> Text
noterClient wid =
    [raw|
function setupNoterClient(wid) {
  const elt = document.getElementById(withWID(wid, "txt"))

  // handle cursor movement
  const selection = {start: 0, end: 0}
  const updateSelection = () => {
    selection.start = elt.selectionStart
    selection.end = elt.selectionEnd
  }
  const isSelecting = () => selection.start < selection.end
  elt.onselectionchange = ev => {
    console.log("onSelectionChange", selection, elt.selectionStart, elt.selectionEnd)
    const prev = selection.start
    updateSelection()
    if (prev != selection.start) {
      butlerDataSocket.send(encodeDataMessage(wid, {move: elt.selectionStart}))
    }
  }

  // handle text change
  elt.oninput = ev => {
    if (ev.inputType == "deleteContentBackward") {
      let size = -1
      if (isSelecting()) {
        size = selection.end - selection.start
      }
      butlerDataSocket.send(encodeDataMessage(wid, {delete: size}))
    } else if (ev.inputType == "deleteContentForward") {
      let size = 1
      if (isSelecting()) {
        size = selection.end - selection.start
      }
      butlerDataSocket.send(encodeDataMessage(wid, {delete: size}))
    } else if (ev.inputType == "insertText" || ev.inputType == "insertFromPaste" || ev.inputType == "insertLineBreak") {
      if (isSelecting()) {
        butlerDataSocket.send(encodeDataMessage(wid, {delete: selection.end - selection.start}))
      }
      let data = ev.data
      if (ev.inputType == "insertLineBreak") {
        data = "\n"
      }
      butlerDataSocket.send(encodeDataMessage(wid, {insert: data}))
    } else {
      console.error("Unknown input", ev)
    }
    updateSelection()
    // butlerDataSocket.send(encodeDataMessage(wid, {insert: ev.data}))
  }

  // handle server event
  butlerDataHandlers[wid] = buf => {
    const body = decodeJSON(buf)
    console.log("Got server event", body)
    // Updating fires the selection changed event to preserve the cursor pos.
    if (body.text !== undefined) {
      elt.value = body.text
      elt.focus()
      elt.setSelectionRange(selection.start, selection.end)
    } else if (body.insert) {
      elt.setRangeText(body.insert, body.pos, body.pos)
    } else if (body.delete) {
      elt.setRangeText("", body.pos, body.pos + body.delete)
    }
  }
}
|]
        <> ("\nsetupNoterClient(" <> showT wid <> ");")
