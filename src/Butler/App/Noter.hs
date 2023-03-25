module Butler.App.Noter (noterApp) where

import Butler
import Butler.Display.Session
import Butler.Display.User
import Butler.Frame
import Butler.Service.FileService

import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Data.Text.Rope qualified as Rope

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
    deriving (Generic, ToJSON)

-- Note EditorAction:
-- Backward delete size shall only be 1. That is because bigger backward deletion requires
-- a selection, which trigger in a @selectionchange@ event that move the cursor at the begining of the chunk to be deleted,
-- resulting in a forward delete action.
data EditorAction
    = MoveCursor Word
    | Insert Text
    | Delete Direction Word

{- | Adjust editors position when a change happens.

For the change author, we need to update its position because the client ignore it's own @selectionchange@ event.
See the @updateSelection@ call done in the @oninput@ handler.

For the other editors, the @setRangeText@ call will trigger the @selectionchange@ event which will update their position after the change is applied.
However, if they are also performing change at the same time, we need to adjust their position.
-}
adjustEditor :: Word -> Editor -> EditorAction -> Editor -> Editor
adjustEditor maxPos author action editor = case action of
    MoveCursor newPos
        | -- This is the editor that moved: set its position
          author.client.endpoint == editor.client.endpoint ->
            editor & #position .~ min maxPos newPos
        | otherwise -> editor
    Insert txt
        | -- The editor is after the insert: move it forward
          editor.position >= author.position ->
            editor & #position .~ editor.position + fromIntegral (Text.length txt)
        | -- The editor is before the insert: don't touch
          otherwise ->
            editor
    Delete Forward count
        | -- The editor is after the removed chunk: move it backward
          editor.position >= author.position + count ->
            editor & #position .~ editor.position - count
        | -- The editor is in the removed chunk: move it at the position of the author
          editor.position > author.position ->
            editor & #position .~ author.position
        | -- The editor is before the delete: don't touch
          otherwise ->
            editor
    Delete Backward count
        | -- The editor is after the removed chunk: move it backward
          editor.position >= author.position ->
            editor & #position .~ editor.position - count
        | -- The editor is in the removed chunk: move it at the new position of the autohr
          editor.position >= author.position - count ->
            editor & #position .~ author.position - count
        | -- The editor is way before the delete: don't touch
          otherwise ->
            editor

data Direction = Forward | Backward
    deriving (Generic, ToJSON)

instance FromJSON EditorAction where
    parseJSON = withObject "EditorAction" \obj -> do
        let
            moveParser = MoveCursor <$> obj .: "move"
            insertParser = Insert <$> obj .: "insert"
            deleteParser = toDelete <$> obj .: "delete"
              where
                toDelete (pos :: Int) = Delete dir (fromIntegral (abs pos))
                  where
                    dir
                        | pos < 0 = Backward
                        | otherwise = Forward
        moveParser <|> deleteParser <|> insertParser

data NoterStatus
    = NewFile
    | EditingFile Directory File
    deriving (Generic, ToJSON)

data NoterState = NoterState
    { status :: NoterStatus
    , dirty :: Bool
    , content :: Rope.Rope
    , editors :: Map Endpoint Editor
    }
    deriving (Generic, ToJSON)

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
                    with textarea_ [wid_ ctx.wid "txt", class_ "w-full h-full"] (toHtmlRaw (Rope.toText state.content))
                script_ (noterClient ctx.wid)

    let handleEditorAction client action = case action of
            MoveCursor pos -> do
                logDebug "cursor moved" ["client" .= client, "pos" .= pos]
                -- TODO: broadcast and render cursor to the other clients
                atomically $ modifyTVar' tState \state ->
                    case Map.lookup client.endpoint state.editors of
                        Nothing -> state
                        Just editor ->
                            let maxPos = Rope.length state.content
                                newEditor = adjustEditor maxPos editor action editor
                             in (state & #editors %~ Map.insert client.endpoint newEditor)
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
                                updateEditors = adjustEditor (Rope.length newContent) editor action
                                newState = state & setContent . (#editors %~ fmap updateEditors) . (#dirty .~ True)
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
            Delete dir deleteSize -> do
                -- logDebug "Deleting" ["count" .= count]
                mBody <- atomically $ stateTVar tState \state ->
                    case Map.lookup client.endpoint state.editors of
                        Nothing -> (Nothing, state)
                        Just editor ->
                            let
                                -- The absolute deletion size
                                deleteStart = case dir of
                                    Backward
                                        | deleteSize > editor.position -> 0
                                        | otherwise -> editor.position - deleteSize
                                    Forward -> editor.position
                                -- The new content
                                (before, rest) = Rope.splitAt deleteStart state.content
                                (_deleted, after) = Rope.splitAt deleteSize rest
                                -- Update state
                                newContent = mconcat [before, after]
                                updateContent = #content .~ newContent
                                updateEditors = adjustEditor (Rope.length newContent) editor action
                                newState = state & updateContent . (#editors %~ fmap updateEditors) . (#dirty .~ True)
                                dirtyChanged
                                    | state.dirty = Nothing
                                    | otherwise = Just newState
                                -- broadcast event
                                body = object ["delete" .= deleteSize, "pos" .= deleteStart]
                             in
                                (Just (dirtyChanged, body), newState)
                case mBody of
                    Just (dirtyChanged, body) -> do
                        sendsBinaryButSelf client ctx.clients (encodeMessageL ctx.wid (encodeJSON body))
                        forM_ dirtyChanged (sendsHtml ctx.clients . fileNameForm)
                    Nothing -> logError "Delete failed" ["client" .= client]

    let modifyEditors edit state =
            let newEditors = edit state.editors
             in (newEditors, state & #editors .~ newEditors)

    forever do
        atomically (readPipe ctx.pipe) >>= \case
            AppDisplay (UserJoined client) -> do
                editors <- atomically $ stateTVar tState (modifyEditors $ Map.insert client.endpoint (Editor client 0))
                atomically $ sendHtml client mountUI
                sendsHtmlButSelf client ctx.clients (editorList editors)
            AppDisplay (UserLeft client) -> do
                editors <- atomically $ stateTVar tState (modifyEditors $ Map.delete client.endpoint)
                sendsHtml ctx.clients (editorList editors)
            AppTrigger ev -> case ev.trigger of
                "refresh" -> atomically do
                    content <- (.content) <$> readTVar tState
                    sendBinary ev.client (encodeMessageL ctx.wid (encodeJSON $ object ["text" .= Rope.toText content]))
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
    // TODO: if adjustEditor is wrong, then we should always send the current position by removing the following check
    if (prev != selection.start) {
      butlerDataSocket.send(encodeDataMessage(wid, {move: elt.selectionStart}))
    }
  }

  // handle text change
  elt.oninput = ev => {
    if (ev.inputType == "deleteContentBackward" || ev.inputType == "deleteContentForward" || ev.inputType == "deleteByCut") {
      let size = 1
      if (isSelecting()) {
        size = selection.end - selection.start
      } else if (ev.inputType == "deleteContentBackward") {
        size = -1
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
