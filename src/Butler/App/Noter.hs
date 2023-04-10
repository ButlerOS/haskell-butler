module Butler.App.Noter (noterApp) where

import Butler
import Butler.Display.Session
import Butler.Display.User
import Butler.Frame
import Butler.Service.FileService

import XStatic.Ace qualified as XStatic

import Data.Map.Strict qualified as Map
import Data.Text.Lines qualified as Lines

noterApp :: App
noterApp =
    (defaultApp "noter" startNoterApp)
        { tags = fromList ["Utility", "Development"]
        , description = "Text editor"
        , start = startNoterApp
        , acceptFiles = Just TextContent
        , xfiles = [XStatic.aceJs]
        , extraXfiles = XStatic.aceBundle
        }

newtype Position = Position (Word, Word)
    deriving newtype (FromJSON)

instance From Position Lines.Position where
    from (Position (posLine, posColumn)) = Lines.Position{posLine, posColumn}

data Editor = Editor
    { client :: DisplayClient
    , position :: Word
    }
    deriving (Generic, ToJSON)

data EditorAction
    = Insert Position Text
    | Delete Position Position

instance FromJSON EditorAction where
    parseJSON = withObject "EditorAction" \obj -> do
        let
            insertParser = Insert <$> obj .: "p" <*> obj .: "i"
            deleteParser = Delete <$> obj .: "p" <*> obj .: "e"
        insertParser <|> deleteParser

data NoterStatus
    = NewFile
    | EditingFile Directory File
    deriving (Generic, ToJSON)

data NoterState = NoterState
    { status :: NoterStatus
    , dirty :: Bool
    , content :: Lines.TextLines
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
                newTVarIO $ NoterState (EditingFile dir file) False (Lines.fromText content) mempty
            _ -> newTVarIO (NoterState NewFile False mempty mempty)

    let updateContent :: Lines.TextLines -> NoterState -> NoterState
        updateContent newContent = (#content .~ newContent) . (#dirty .~ True)

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
                with div_ [wid_ ctx.wid "txt", class_ "absolute bottom-0 top-10 left-0 right-0"] (toHtmlRaw (Lines.toText state.content))
                script_ (noterClient ctx.wid)

    let handleEditorAction rawBuffer client action = case action of
            Insert pos txt -> do
                mDirty <- atomically $ stateTVar tState \state ->
                    let
                        -- update content
                        (before, after) = Lines.splitAtPosition (from pos) state.content
                        insertRope = Lines.fromText txt
                        newContent = mconcat [before, insertRope, after]
                        newState = updateContent newContent state
                        dirtyChanged
                            | state.dirty = Nothing
                            | otherwise = Just newState
                     in
                        (Just dirtyChanged, newState)
                case mDirty of
                    Just dirtyChanged -> do
                        sendsBinaryButSelf client ctx.shared.clients rawBuffer
                        forM_ dirtyChanged (sendsHtml ctx.shared.clients . fileNameForm)
                    Nothing -> logError "Insert failed" ["client" .= client, "txt" .= txt]
            Delete start end -> do
                -- logDebug "Deleting" ["count" .= count]
                mDirty <- atomically $ stateTVar tState \state ->
                    let
                        -- The new content
                        (before, _rest) = Lines.splitAtPosition (from start) state.content
                        (_deleted, after) = Lines.splitAtPosition (from end) state.content
                        -- Update state
                        newContent = mconcat [before, after]
                        newState = updateContent newContent state
                        dirtyChanged
                            | state.dirty = Nothing
                            | otherwise = Just newState
                     in
                        (Just dirtyChanged, newState)
                case mDirty of
                    Just dirtyChanged -> do
                        sendsBinaryButSelf client ctx.shared.clients rawBuffer
                        forM_ dirtyChanged (sendsHtml ctx.shared.clients . fileNameForm)
                    Nothing -> logError "Delete failed" ["client" .= client]

    let modifyEditors edit state =
            let newEditors = edit state.editors
             in (newEditors, state & #editors .~ newEditors)

    forever do
        atomically (readPipe ctx.pipe) >>= \case
            AppDisplay (UserJoined client) -> do
                editors <- atomically $ stateTVar tState (modifyEditors $ Map.insert client.endpoint (Editor client 0))
                atomically $ sendHtml client mountUI
                sendsHtmlButSelf client ctx.shared.clients (editorList editors)
            AppDisplay (UserLeft client) -> do
                editors <- atomically $ stateTVar tState (modifyEditors $ Map.delete client.endpoint)
                sendsHtml ctx.shared.clients (editorList editors)
            AppTrigger ev -> case ev.trigger of
                "refresh" -> atomically do
                    content <- (.content) <$> readTVar tState
                    sendBinary ev.client (encodeMessage (from ctx.wid) (encodeJSON $ object ["text" .= Lines.toText content]))
                "save-file" -> do
                    state <- readTVarIO tState
                    case state.status of
                        NewFile -> logError "Need a file name" ["ev" .= ev]
                        EditingFile dir file -> do
                            writeFileBS dir file (encodeUtf8 $ Lines.toText state.content)
                            let newState = state & (#dirty .~ False)
                            atomically $ writeTVar tState newState
                            sendsHtml ctx.shared.clients (fileNameForm newState)
                "new-file" -> case ev.body ^? key "file" . _JSON of
                    Just fp -> do
                        state <- readTVarIO tState
                        mDir <- case state.status of
                            NewFile -> atomically $ newDirectory rootDir "Documents"
                            EditingFile curDir _ -> pure (Just curDir)
                        case mDir of
                            Just dir -> do
                                file <- createFile dir fp (encodeUtf8 $ Lines.toText state.content)
                                let newState = state & (#status .~ EditingFile dir file) . (#dirty .~ False)
                                atomically do
                                    writeTVar tState newState
                                    modifyMemoryVar memFile (const $ getFileLoc dir (Just file))
                                sendsHtml ctx.shared.clients (fileNameForm newState)
                            Nothing -> logError "Documents is not a directory" []
                    Nothing -> logError "Invalid new-file" ["ev" .= ev]
                _ -> logError "Unknown ev" ["ev" .= ev]
            AppData ev -> case decodeJSON @EditorAction (from ev.buffer) of
                Just action -> handleEditorAction (from ev.rawBuffer) ev.client action
                Nothing -> logError "Unknown action" ["ev" .= ev]
            AppFile dir (Just file) -> do
                content <- decodeUtf8 <$> readFileBS dir file
                atomically do
                    modifyMemoryVar memFile (const $ getFileLoc dir (Just file))
                    let newContent = Lines.fromText content
                    writeTVar tState $ NoterState (EditingFile dir file) False newContent
                sendsHtml ctx.shared.clients mountUI
            _ -> pure ()

-- See https://ace.c9.io/#nav=howto and https://ajaxorg.github.io/ace-api-docs/classes/Ace.EditSession.html
noterClient :: AppID -> Text
noterClient wid =
    [raw|
function setupNoterClient(wid) {
  const elt = document.getElementById(withWID(wid, "txt"))
  const editor = ace.edit(elt)

  let localEvent = false
  editor.setTheme("ace/theme/monokai");
  editor.resize()
  editor.addEventListener("change", ev => {
    if (!localEvent) {
      const msg = {p: [ev.start.row, ev.start.column]}
      if (ev.action === "insert") {
        msg.i = ev.lines.join("\n")
      } else if (ev.action == "remove") {
        msg.e = [ev.end.row, ev.end.column]
      } else {
        console.error("Unknown event:", ev)
      }
      butlerDataSocket.send(encodeDataMessage(wid, msg))
    }
  })

  const getPoint = arr => ({row: arr[0], column: arr[1]})

  // handle server event
  butlerDataHandlers[wid] = buf => {
    const body = decodeJSON(buf)
    console.log("Got server event", body)
    localEvent = true
    try {
      if (body.text !== undefined) {
        editor.setValue(body.text)
        editor.focus()
      } else if (body.i) {
        console.log("Inserting", getPoint(body.p), body.i)
        editor.session.insert(getPoint(body.p), body.i)
      } else if (body.e) {
        editor.session.remove({start: getPoint(body.p), end: getPoint(body.e)})
      }
    } finally {
      localEvent = false
    }
  }
}
|]
        <> ("\nsetupNoterClient(" <> showT wid <> ");")
