module Butler.App.Noter (noterApp) where

import Butler
import Butler.Display.Session
import Butler.Display.User
import Butler.Frame
import Butler.Service.FileService

import XStatic.Ace qualified as XStatic

import Data.ByteString qualified as BS
import Data.List qualified
import Data.Text.Lines qualified as Lines

import Butler.App
import Data.Map.Strict qualified as Map

noterApp :: App
noterApp =
    (defaultApp "noter" startNoterApp)
        { tags = fromList ["Utility", "Development"]
        , description = "Text editor"
        , start = startNoterApp
        , acceptFiles = Just TextContent
        , xfiles = XStatic.aceJs : maybeToList linkExt
        , extraXfiles = XStatic.aceBundle
        }
  where
    linkExt = Data.List.find (\xf -> "ext-linking.js" `BS.isInfixOf` xf.xfPath) XStatic.aceBundle

newtype Position = Position (Word, Word)
    deriving newtype (ToJSON, FromJSON, Eq)

instance From Position Lines.Position where
    from (Position (posLine, posColumn)) = Lines.Position{posLine, posColumn}

data Editor = Editor
    { client :: DisplayClient
    , selection :: (Position, Maybe Position)
    }
    deriving (Generic, ToJSON)

newEditor :: DisplayClient -> Editor
newEditor client = Editor client (Position (0, 0), Nothing)

data EditorAction
    = Insert Position Text
    | Delete Position Position
    | Select Position (Maybe Position)

instance FromJSON EditorAction where
    parseJSON = withObject "EditorAction" \obj -> do
        let
            insertParser = Insert <$> obj .: "p" <*> obj .: "i"
            deleteParser = Delete <$> obj .: "p" <*> obj .: "e"
            selectParser = Select <$> obj .: "p" <*> obj .:? "s"
        insertParser <|> deleteParser <|> selectParser

data NoterStatus
    = NewFile
    | EditingFile Directory File
    deriving (Generic, ToJSON)

data NoterState = NoterState
    { status :: NoterStatus
    , dirty :: Bool
    , content :: Lines.TextLines
    }
    deriving (Generic, ToJSON)

startNoterApp :: AppContext -> ProcessIO ()
startNoterApp ctx = do
    (currentFile, memFile) <- newAppMemory ctx.wid "noter-file" mempty
    rootDir <- getVolumeDirectory ctx.shared Nothing
    tState <-
        resolveFileLoc rootDir currentFile >>= \case
            Just (dir, Just file) -> do
                content <- decodeUtf8 <$> readFileBS dir file
                newTVarIO $ NoterState (EditingFile dir file) False (Lines.fromText content)
            _ -> newTVarIO (NoterState NewFile False "")

    editors <- atomically newClientsData

    -- Return the current file name extension
    let editingExt state = case state.status of
            NewFile -> mempty
            EditingFile _ file -> BS.takeWhileEnd (/= 46) file.name

    -- Handle the connection to a REPL
    (tmREPLBuffer :: TVar (Maybe (MVar LByteString))) <- newTVarIO Nothing
    (tmREPLBaton :: MVar ()) <- newEmptyMVar
    let initREPL :: AppInstance -> ProcessIO ()
        initREPL appInstance = do
            logInfo "Initializing REPL" []
            ext <- editingExt <$> readTVarIO tState
            let
                cmd :: [Text]
                cmd
                    | ext == "js" = ["node"]
                    | ext == "hs" = ["ghci"]
                    | ext == "py" = ["python3"]
                    | otherwise = ["node"]
            replBuffer <- fromMaybe (error "bad buffer?") <$> appCall appInstance "start-repl" (toJSON cmd)
            atomically $ writeTVar tmREPLBuffer (Just replBuffer)
            doUpdateREPL

        debounceUpdateREPL = forever do
            takeMVar tmREPLBaton
            sleep 200
            whenM (isEmptyMVar tmREPLBaton) do
                doUpdateREPL

        doUpdateREPL :: ProcessIO ()
        doUpdateREPL =
            readTVarIO tmREPLBuffer >>= \case
                Nothing -> logError "Sending buffer failed: Not connected to the REPL" []
                Just replBuffer -> do
                    content <- from . encodeUtf8 . Lines.toText . (.content) <$> readTVarIO tState
                    putMVar replBuffer content

        updateREPL :: ProcessIO ()
        updateREPL = void $ tryPutMVar tmREPLBaton ()

    spawnThread_ debounceUpdateREPL

    -- restore the REPL
    (mREPLAppId :: Maybe AppID, memREPLAppId) <- newAppMemory ctx.wid "noter-repl" Nothing
    forM_ mREPLAppId \replAppId -> do
        mApp <-
            atomically =<< waitTransaction 500 do
                Map.lookup replAppId <$> getApps ctx.shared.apps >>= \case
                    Nothing -> retrySTM
                    Just a -> pure a
        case mApp of
            WaitTimeout -> logError "Couldn't find REPL app instance" ["id" .= replAppId]
            WaitCompleted app -> initREPL app

    let updateContent :: Lines.TextLines -> NoterState -> NoterState
        updateContent newContent = (#content .~ newContent) . (#dirty .~ True)

    let saveBtn = withTrigger_ "click" ctx.wid "save-file" button_ [class_ ("mx-2 " <> btnGreenClass)] "Save"
        fileNameInput = withTrigger_ "" ctx.wid "new-file" (input_ []) [type_ "text", placeholder_ "File name", name_ "file"]
        editorList =
            with div_ [wid_ ctx.wid "editors"] do
                xs <- lift (getClientsData editors)
                forM_ xs \editor -> do
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
            let extension = editingExt state
            with div_ [wid_ ctx.wid "w", class_ "w-full h-full flex flex-col"] do
                with div_ [class_ "flex flex-row"] do
                    withTrigger_ "click" ctx.wid "refresh" i_ [class_ "ri-refresh-line cursor-pointer mx-1"] mempty
                    fileNameForm state
                    when (extension `elem` ["js", "hs", "py"]) do
                        withTrigger_ "click" ctx.wid "start-repl" i_ [] "R"
                    editorList
                with div_ [wid_ ctx.wid "txt", class_ "absolute bottom-0 top-10 left-0 right-0"] (toHtmlRaw (Lines.toText state.content))
                let mode
                        | extension == "js" = "ace/mode/javascript"
                        | extension == "py" = "ace/mode/python"
                        | extension == "hs" = "ace/mode/haskell"
                        | otherwise = "ace/mode/markdown"
                script_ (noterClient mode ctx.wid)

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
                updateREPL
                case mDirty of
                    Just dirtyChanged -> do
                        sendsBinaryButSelf client ctx.shared.clients rawBuffer
                        forM_ dirtyChanged (sendsHtml ctx.shared.clients . fileNameForm)
                    Nothing -> logError "Insert failed" ["client" .= client, "txt" .= txt]
            Delete start end -> do
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
                updateREPL
                case mDirty of
                    Just dirtyChanged -> do
                        sendsBinaryButSelf client ctx.shared.clients rawBuffer
                        forM_ dirtyChanged (sendsHtml ctx.shared.clients . fileNameForm)
                    Nothing -> logError "Delete failed" ["client" .= client]
            Select start mEnd -> withClientsData editors client \tEditor -> do
                hasChanged <- atomically $ stateTVar tEditor \editor ->
                    let newSelection = (start, mEnd)
                     in (newSelection /= editor.selection, editor & #selection .~ newSelection)
                when hasChanged do
                    let withEnd = case mEnd of
                            Nothing -> id
                            Just e -> ("e" .= e :)
                        msg = object $ withEnd ["c" .= client.process.pid, "p" .= start]
                     in sendsBinaryButSelf client ctx.shared.clients (encodeMessage (from ctx.wid) (encodeJSON msg))
                logInfo "Selecting" ["changed" .= hasChanged, "start" .= start, "end" .= mEnd]

    forever do
        atomically (readPipe ctx.pipe) >>= \case
            AppDisplay (UserJoined client) -> do
                atomically $ addClientsData editors client (newEditor client)
                atomically $ sendHtml client mountUI
                sendsHtmlButSelf client ctx.shared.clients editorList
            AppDisplay (UserLeft client) -> do
                atomically $ delClientsData editors client
                sendsHtml ctx.shared.clients editorList
            AppTrigger ev -> case ev.trigger of
                "refresh" -> do
                    atomically do
                        content <- (.content) <$> readTVar tState
                        sendBinary ev.client (encodeMessage (from ctx.wid) (encodeJSON $ object ["text" .= Lines.toText content]))
                    updateREPL
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
                            NewFile -> newDirectory rootDir "Documents"
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
                "start-repl" -> do
                    mShellApp <- Map.lookup shellAppID <$> atomically (getApps ctx.shared.apps)
                    forM_ mShellApp \shellApp -> do
                        mInstance <- appCall shellApp "start-app" (object ["name" .= ("termREPL" :: Text)])
                        logInfo "Registering REPL" ["wid" .= ((.wid) <$> mInstance)]
                        forM_ mInstance \(appInstance :: AppInstance) -> do
                            initREPL appInstance
                            atomically do
                                modifyMemoryVar memREPLAppId (const $ Just appInstance.wid)
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
noterClient :: Text -> AppID -> Text
noterClient mode wid =
    [raw|
function setupNoterClient(mode, wid) {
  const elt = document.getElementById(withWID(wid, "txt"))
  const editor = ace.edit(elt)

  editor.setTheme("ace/theme/monokai");
  // TODO: set this through server event when file name change
  editor.session.setMode(mode);
  editor.setOptions({
    enableLinking: true
  });
  editor.resize()
  editor.on("linkClick",function(data){
    const regex = /https?:\/\/[^\s"']+/g;
    const links = data.token.value.match(regex)
    const link = links ? links[0] : null;
    console.log("Clicked on '", data.token.value, "', link: ", link);
    if (link) {
      window.open(link, "_blank");
    }
  });

  let localEvent = false
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
      sendJSONMessage(wid, msg)
    }
  })

  editor.addEventListener("changeSelection", debounceData(100, () => {
    const {start,end} = editor.getSelectionRange()
    const msg = {p: [start.row, start.column]}
    if (start.row !== end.row || start.column !== end.column) {
      msg.s = [end.row, end.column]
    }
    sendJSONMessage(wid, msg)
  }))

  const getPoint = arr => ({row: arr[0], column: arr[1]})

  const cursors = {}
  const markerUpdate = (html, markerLayer, session, config) => {
    var start = config.firstRow, end = config.lastRow;
    for (const cursor of Object.values(cursors)) {
        if (cursor.start.row < start || cursor.start.row > end) {
            continue
        } else {
            const pos = cursor.toScreenRange(session)
            if (pos.start.row == pos.end.row) {
              markerLayer.drawSingleLineMarker(html, pos, "ace_selected-word", config)
            } else {
              markerLayer.drawMultiLineMarker(html, pos, "ace_selected-word", config)
            }

        }
    }
  }
  const redrawCursors = () => {
   editor.session._signal("changeFrontMarker");
  }
  const setCursor = (pid, start, mEnd) => {
    if (mEnd === undefined) {
      mEnd = start
    }
    cursors[pid] = new ace.Range(start[0], start[1], mEnd[0], mEnd[1])
    redrawCursors()
  }
  const delCursor = (pid) => {
    delete cursors[pid]
    redrawCursors()
  }
  editor.session.addDynamicMarker({update: markerUpdate}, true)

  // handle server event
  butlerDataHandlers[wid] = buf => {
    const body = decodeJSON(buf)
    // console.log("Got server event", body)
    localEvent = true
    try {
      if (body.text !== undefined) {
        const prevSelection = editor.getSelectionRange()
        editor.setValue(body.text)
        editor.focus()
        editor.navigateTo(prevSelection.start.row, prevSelection.start.column)
      } else if (body.i) { // Insert event
        editor.session.insert(getPoint(body.p), body.i)
      } else if (body.c) { // Cursor event
        if (body.p) { // Cursor moved
          setCursor(body.c, body.p, body.e)
        } else { // Cursor removed
          delCursor(body.c)
        }
      } else if (body.e) { // Remove event
        editor.session.remove({start: getPoint(body.p), end: getPoint(body.e)})
      } else {
        console.error("Unknown event", body)
      }
    } finally {
      localEvent = false
    }
  }

  onWindowResize[wid] = (x, y) => {
    editor.resize(true)
  }
}
|]
        <> ("\nsetupNoterClient(" <> showT mode <> ", " <> showT wid <> ");")
