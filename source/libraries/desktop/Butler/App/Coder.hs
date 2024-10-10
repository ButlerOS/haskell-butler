module Butler.App.Coder (coderApp) where

import Butler
import Butler.Core (writePipe)
import Butler.Core.NatMap qualified as NM
import Butler.Display.Session
import Butler.Display.User
import Butler.Frame
import Butler.Service.FileService

import XStatic.CodeMirror qualified as XStatic
import XStatic.OTJS qualified as XStatic

import Data.ByteString qualified as BS

import Butler.App
import Data.Map.Strict qualified as Map

import Control.OperationalTransformation.Selection qualified as OT
import Control.OperationalTransformation.Server qualified as OT
import Control.OperationalTransformation.Text qualified as OT

coderApp :: App
coderApp =
    (defaultApp "coder" startNoterApp)
        { tags = fromList ["Utility", "Development"]
        , description = "Code editor"
        , start = startNoterApp
        , acceptFiles = Just TextContent
        , xfiles = XStatic.otJs : XStatic.codemirror
        }

-- | Event from the client to the server
data ServerRequest
    = ClientOp (OT.Revision, OT.TextOperation)
    | ClientSelection (Maybe OT.Range)
    deriving stock (Show)

-- | Event from the server to the client
data ServerResponse
    = -- | Reset (optional error message, current revision, current body)
      Reset {mErr :: Maybe String, rev :: OT.Revision, body :: Text}
    | -- | Apply an operation
      ApplyOp (OT.Revision, OT.TextOperation)
    | NewSelection {cid :: ClientID, mRange :: Maybe OT.Range}
    | NewClient {cid :: ClientID, name :: Text, color :: Text}
    | DelClient ClientID
    | DoAck OT.Revision

data NoterStatus
    = NewFile
    | EditingFile Directory File
    deriving (Generic, ToJSON)

data NoterState = NoterState
    { status :: NoterStatus
    , dirty :: Bool
    , content :: OTServer
    }
    deriving (Generic)

data Editor = Editor
    { clientID :: Natural
    , client :: DisplayClient
    , mRange :: Maybe OT.Range
    }
    deriving (Generic)

type OTServer = OT.ServerState Text OT.TextOperation
type ClientID = Natural

startNoterApp :: AppContext -> ProcessIO ()
startNoterApp ctx = do
    let sendEvent :: DisplayClient -> ServerResponse -> STM ()
        sendEvent client ev = do
            sendBinary client (encodeMessage (from ctx.wid) (encodeJSON ev))

        broadcastEvent :: DisplayClient -> ServerResponse -> ProcessIO ()
        broadcastEvent client ev = do
            let msg = encodeMessage (from ctx.wid) (encodeJSON ev)
            sendsBinaryButSelf client ctx.shared.clients msg

    (currentFile, memFile) <- newAppMemory ctx.wid "noter-file" mempty
    rootDir <- getVolumeDirectory ctx.shared Nothing
    tState <-
        resolveFileLoc rootDir currentFile >>= \case
            Just (dir, Just file) -> do
                content <- decodeUtf8 <$> readFileBS dir file
                newTVarIO $ NoterState (EditingFile dir file) False (OT.initialServerState content)
            _ -> newTVarIO (NoterState NewFile False (OT.initialServerState ""))

    let getContentRev :: STM (OT.Revision, Text)
        getContentRev = do
            OT.ServerState rev body _op <- (.content) <$> readTVar tState
            pure (rev, body)

    -- Return the current file name extension
    let editingExt state = case state.status of
            NewFile -> mempty
            EditingFile _ file -> BS.takeWhileEnd (/= 46) file.name

    -- Handle the connection to a REPL
    (tmREPLApp :: TVar (Maybe AppInstance)) <- newTVarIO Nothing
    (tmREPLBaton :: MVar ()) <- newEmptyMVar
    let initREPL :: AppInstance -> ProcessIO ()
        initREPL appInstance = do
            ext <- editingExt <$> readTVarIO tState
            let
                commandArgs :: [String]
                commandArgs
                    | ext == "js" = ["node"]
                    | ext == "hs" = ["ghci"]
                    | ext == "py" = ["python3"]
                    | otherwise = ["node"]

            logInfo "Initializing REPL" ["ext" .= decodeUtf8 ext, "cmd" .= commandArgs]
            appCall appInstance "start-repl" (ctx, commandArgs) >>= \case
                Just () -> do
                    atomically $ writeTVar tmREPLApp (Just appInstance)
                    doUpdateREPL
                Nothing -> logError "Didn't received repl acknowledgement" []

        debounceUpdateREPL = forever do
            takeMVar tmREPLBaton
            sleep 200
            whenM (isEmptyMVar tmREPLBaton) do
                doUpdateREPL

        doUpdateREPL :: ProcessIO ()
        doUpdateREPL =
            readTVarIO tmREPLApp >>= \case
                Nothing -> pure ()
                Just replApp -> do
                    mvReply <- newEmptyTMVarIO
                    body <- toDyn <$> atomically getContentRev
                    writePipe replApp.pipe (AppSync (SyncEvent "new-doc" body mvReply))

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

    let saveBtn = withTrigger_ "click" ctx.wid "save-file" button_ [class_ ("mx-2 " <> btnGreenClass)] "Save"
        fileNameInput = withTrigger_ "" ctx.wid "new-file" (input_ []) [type_ "text", placeholder_ "File name", name_ "file"]
        editorList =
            with div_ [wid_ ctx.wid "editors"] do
                xs <- lift (getClients ctx.shared.clients)
                forM_ xs \client -> do
                    userIcon =<< lift (readTVar client.session.username)
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
            renderTaskBar ctx.wid mempty $
                let title = case state.status of
                        NewFile -> showT ctx.wid
                        EditingFile _ file -> decodeUtf8 $ BS.take 24 file.name
                 in toHtml $ "N<" <> title <> ">"

            with div_ [wid_ ctx.wid "w", class_ "w-full h-full flex flex-col"] do
                with div_ [class_ "flex flex-row border-b border-indigo-500"] do
                    withTrigger_ "click" ctx.wid "refresh" i_ [class_ "ri-refresh-line cursor-pointer mx-1"] mempty
                    fileNameForm state
                    when (extension `elem` ["js", "hs", "py"]) do
                        withTrigger_ "click" ctx.wid "start-repl" i_ [] "R"
                    when (extension == "md") do
                        withTrigger_ "click" ctx.wid "start-md2jira" i_ [] "R"
                    editorList
                with div_ [id_ "toolbar", class_ "hidden"] do
                    pure ()
                style_ ".cm-editor { height: 100% }\n"
                with div_ [wid_ ctx.wid "editor", class_ ""] do
                    pure ()
                script_ (noterClient ctx.wid)

    let
        -- update coming from md2jira
        handleAppUpdate rev op = do
            logInfo "System update..." ["op" .= op]
            state <- readTVarIO tState
            case OT.applyOperation state.content rev op () of
                Left err -> logError "Fail to apply system op" ["rev" .= rev, "op" .= op, "err" .= err]
                Right (newOp, _newOTCursors, newServerState) -> do
                    let newState = state{content = newServerState, dirty = True}
                    atomically $ writeTVar tState newState
                    let OT.ServerState newRev _newDoc _op = newServerState
                    let msg = encodeMessage (from ctx.wid) $ encodeJSON $ ApplyOp (newRev, newOp)
                    sendsBinary ctx.shared.clients msg
                    unless state.dirty do
                        sendsHtml ctx.shared.clients (fileNameForm newState)

    cursors <- atomically newClientsData
    let handleEditorAction client request = case request of
            ClientOp (rev, op) -> do
                state <- readTVarIO tState
                case OT.applyOperation state.content rev op () of
                    Left err -> do
                        logError "Fail to apply op" ["client" .= client, "rev" .= rev, "op" .= op]
                        atomically $ resetClient err state.content
                    Right (newOp, _newOTCursors, newServerState) -> do
                        let newState = state{content = newServerState, dirty = True}
                        atomically $ writeTVar tState newState
                        let OT.ServerState newRev _newDoc _op = newServerState
                        -- logDebug "new state" ["client" .= client, "doc" .= _newDoc]
                        updateREPL
                        atomically $ sendEvent client $ DoAck newRev
                        broadcastEvent client $ ApplyOp (newRev, newOp)
                        unless state.dirty do
                            sendsHtml ctx.shared.clients (fileNameForm newState)
            ClientSelection mRange -> withClientsData cursors client \vEditor -> do
                cid <- atomically do
                    editor <- readTVar vEditor
                    modifyTVar vEditor (#mRange .~ mRange)
                    pure editor.clientID
                logDebug "New selection" ["client" .= client, "range" .= mRange]
                broadcastEvent client NewSelection{cid, mRange}
          where
            resetClient err state = do
                let OT.ServerState rev doc _ = state
                sendEvent client $ Reset (Just err) rev doc

    counter <- atomically NM.newNatCounter
    let newEditor client = do
            clientID <- NM.incr counter
            let editor = Editor{clientID, client, mRange = Nothing}
            addClientsData cursors client editor
            sendHtml client mountUI
            (rev, body) <- getContentRev
            sendEvent client $ Reset Nothing rev body
            pure editor

        mkNewClientEvent editor = do
            userName <- readTVar editor.client.session.username
            let color = userColor userName
                name = into @Text userName
                cid = editor.clientID
            pure NewClient{cid, name, color}

    let handleREPLEvent appName = do
            mShellApp <- Map.lookup shellAppID <$> atomically (getApps ctx.shared.apps)
            forM_ mShellApp \shellApp -> do
                mInstance <- appCall shellApp "start-app" (object ["name" .= (appName :: Text)])
                logInfo "Registering REPL" ["wid" .= ((.wid) <$> mInstance)]
                forM_ mInstance \(appInstance :: AppInstance) -> do
                    initREPL appInstance
                    atomically do
                        modifyMemoryVar memREPLAppId (const $ Just appInstance.wid)
    forever do
        atomically (readPipe ctx.pipe) >>= \case
            AppDisplay (UserJoined client) -> do
                logInfo "got user" []
                newClientEvent <- atomically (mkNewClientEvent =<< newEditor client)
                editors <- atomically $ getClientsData cursors
                forM_ editors \editor ->
                    when (editor.client.endpoint /= client.endpoint) do
                        -- Send the new client to the other client
                        atomically $ sendEvent editor.client newClientEvent
                        -- Send the other client to the new client
                        atomically $ sendEvent client =<< mkNewClientEvent editor
                        -- Send the other client cursor to the new client
                        forM_ editor.mRange \range -> do
                            atomically $ sendEvent client $ NewSelection editor.clientID (Just range)

                -- Update the editor list widget
                sendsHtmlButSelf client ctx.shared.clients editorList
            AppDisplay (UserLeft client) -> do
                withClientsData cursors client \tEditor -> do
                    -- Remove client cursor from other clients
                    editor <- readTVarIO tEditor
                    broadcastEvent client $ DelClient editor.clientID
                -- Remove client cursor from server state
                atomically $ delClientsData cursors client
                -- Update the editor list widget
                sendsHtml ctx.shared.clients editorList
            AppTrigger ev -> case ev.trigger of
                "refresh" -> do
                    atomically do
                        OT.ServerState rev body _op <- (.content) <$> readTVar tState
                        sendEvent ev.client $ Reset Nothing rev body
                    updateREPL
                "save-file" -> do
                    state <- readTVarIO tState
                    case state.status of
                        NewFile -> logError "Need a file name" ["ev" .= ev]
                        EditingFile dir file -> do
                            let OT.ServerState _ body _ = state.content
                            writeFileBS dir file (encodeUtf8 body)
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
                                body <- snd <$> atomically getContentRev
                                file <- createFile dir fp (encodeUtf8 body)
                                let newState = state & (#status .~ EditingFile dir file) . (#dirty .~ False)
                                atomically do
                                    writeTVar tState newState
                                    modifyMemoryVar memFile (const $ getFileLoc dir (Just file))
                                sendsHtml ctx.shared.clients (fileNameForm newState)
                            Nothing -> logError "Documents is not a directory" []
                    Nothing -> logError "Invalid new-file" ["ev" .= ev]
                "start-repl" -> handleREPLEvent "termREPL"
                "start-md2jira" -> handleREPLEvent "md2jira"
                _ -> logError "Unknown ev" ["ev" .= ev]
            AppData ev -> case decodeJSON @ServerRequest (from ev.buffer) of
                Just action -> handleEditorAction ev.client action
                Nothing -> logError "Unknown action" ["ev" .= ev]
            AppSync ev -> case ev.name of
                "update-doc" | Just (rev, ops) <- fromDynamic @(OT.Revision, OT.TextOperation) ev.message -> do
                    handleAppUpdate rev ops
                    updateREPL
                _ -> logError "Bad sync" ["action" .= ev.name]
            AppFile dir (Just file) -> do
                content <- decodeUtf8 <$> readFileBS dir file
                atomically do
                    modifyMemoryVar memFile (const $ getFileLoc dir (Just file))
                    writeTVar tState $ NoterState (EditingFile dir file) False (OT.initialServerState content)
                -- TODO: ensure this happen before any user join
                pure ()
            _ -> pure ()

noterClient :: AppID -> Text
noterClient wid =
    [raw|
// CodeMirror uses the following delta encoding:
// - insert: [0, "text"]
// - retain: count
// - delete: [count]

// ot.hs use the following encoding:
// - "text"
// - count (for retain)
// - -count (for delete)

// Convert delta from CodeMirror to ot.hs
function encodeDelta(d) {
  if (d.length) {
    if (d.length == 1) {
      return d[0] * -1
    } else if (d.length >= 2) {
      const ins = d.slice(1).join("\n")
      if (d[0] == 0)
        return ins
      return [d[0] * -1, ...ins]
    } else {
      throw ("Unknown delta array:" + d)
    }
  } else {
    return d
  }
}

// Convert delta from ot.hs to Quill
function decodeDelta(d) {
  if (typeof d === 'string' || d instanceof String) {
    return [0, ...d.split("\n")]
  } else if (d > 0) {
    return d
  } else {
    return [d * -1]
  }
}

function foldAllHeadings(body) {
  const foldRanges = [];
  const addFoldRange = (from, to) => {
    if (from !== null && from < to) foldRanges.push({from, to})
  }
  let pos = 0;
  let begin = null;
  let epicBegin = null;
  body.split('\n').forEach(l => {
    if (l.startsWith("## ")) {
      addFoldRange(begin, pos - 1)
      begin = pos + l.length
    } else if (l.startsWith("# ")) {
      addFoldRange(begin, pos - 2)
      begin = null
    }
    pos += l.length + 1
  })
  addFoldRange(begin, pos - 1)
  return {
    effects: foldRanges.map(range => CodeMirror.foldEffect.of({ from: range.from, to: range.to }))
  };
}

function setupNoterClient(wid) {
  const elt = document.getElementById(withWID(wid, "editor"))

  // Setup editor instance
  const otPlugin =  CodeMirror.ViewPlugin.fromClass(class {
    constructor(view) {}
    update(update) {
      if (update.docChanged) {
        update.transactions.forEach(tr => {
          if (tr.annotations.indexOf("remote") == -1) {
            const op = ot.TextOperation.fromJSON(tr.changes.toJSON().flatMap(encodeDelta))
            client.applyClient(op)
          }
        })
      }
    }
  });

  // Setup codemirror editor instance
  const editor = new CodeMirror.EditorView({
    extensions: [CodeMirror.basicSetup, CodeMirror.markdown(), otPlugin],
    parent: elt,
  })

  const newClient = (rev) => {
    client = new ot.Client(rev)
    client.sendOperation = (rev, op) => {
      // console.log("client sending", rev, op)
      sendJSONMessage(wid, [rev, op])
    }
    client.applyOperation = (op) => {
      // console.log("client apply", op)
      const edit = CodeMirror.ChangeSet.fromJSON(op.ops.map(decodeDelta));
      const transaction = editor.viewState.state.update({changes: edit, annotations: ["remote"]})
      editor.dispatch(transaction)
    }
  }

  // Handle server events
  butlerDataHandlers[wid] = (buf) => {
    const msg = decodeJSON(buf)
    // console.log("Got event:", JSON.stringify(msg))

    if ('cid' in msg) {
      // TODO: implement cursor event
    } else if ('rev' in msg) {
      // sync event
      if ('body' in msg) {
        // Reset local state
        newClient(msg.rev)
        editor.dispatch({changes: {
          from: 0,
          to: editor.state.doc.length,
          insert: msg.body
        }, annotations: ["remote"]})
        editor.dispatch(foldAllHeadings(msg.body))
      } else {
        // Rev without body is an acknowledge request
        client.serverAck()
      }
      if ('err' in msg) {
        console.error("Got an error: " + msg.err)
      }
    } else if (msg.length == 2) {
      // delta event
      const [rev, delta] = msg
      client.applyServer(ot.TextOperation.fromJSON(delta))
    }
  };

  globalEditors[wid] = {
    scroll: (target) => {
      const goToLine = (count, iter) => {
        if (!iter.done)
          return iter.value.search(target) != -1 ?
              count
            : goToLine(count + 1, iter.next())
      }
      const pos = goToLine(0, editor.state.doc.iterLines())
      const line = editor.state.doc.line(pos)
      editor.dispatch({
        selection: { head: line.from, anchor: line.from },
        effects: CodeMirror.EditorView.scrollIntoView(line.from, {
          y: "start",
          yMargin: 40,
        })
      })
    }
  }
  onElementRemoved(document.getElementById(withWID(wid, "w")), () => {
    delete globalEditors[wid]
  })
}
|]
        <> ("\nsetupNoterClient(" <> showT wid <> ");")

instance ToJSON ServerResponse where
    toJSON = \case
        Reset{mErr, rev, body} -> toJSON $ object $ maybe id (\e -> ("err" .= e :)) mErr ["rev" .= rev, "body" .= body]
        ApplyOp tup -> toJSON tup
        -- TODO: no need to send the revision on ack
        DoAck rev -> toJSON $ object ["rev" .= rev]
        NewSelection{cid, mRange} -> toJSON $ object ["cid" .= cid, "sel" .= mRange]
        NewClient{cid, name, color} -> toJSON $ object ["cid" .= cid, "name" .= name, "color" .= color]
        DelClient cid -> toJSON $ object ["cid" .= cid]

instance FromJSON ServerRequest where
    parseJSON v = (ClientOp <$> parseJSON v) <|> (ClientSelection <$> parseJSON v)
