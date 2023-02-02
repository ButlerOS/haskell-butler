module Butler.App.Terminal (termApp) where

import Butler.Prelude

import System.Posix.Pty qualified as Pty

import Butler
import Butler.Desktop
import Butler.Logger
import Butler.WebSocket

import System.Process (cleanupProcess)
import System.Process.Typed qualified

data XtermServer = XtermServer
    { inputChan :: TChan ByteString
    , dimension :: TVar (Maybe (Int, Int))
    }

newXtermServer :: STM XtermServer
newXtermServer = XtermServer <$> newTChan <*> newTVar Nothing

renderApp :: WinID -> Natural -> Natural -> XtermServer -> HtmlT STM ()
renderApp wid chan dimChanID server =
    with div_ [wid_ wid "w", class_ "w-full h-full border-2 border-indigo-600"] do
        dimM <- lift (readTVar server.dimension)
        script_ (termClient wid chan dimChanID dimM)

renderTray :: XtermServer -> DisplayClient -> HtmlT STM ()
renderTray server _ = do
    with span_ [id_ "term-tray", class_ "inline-block bg-stone-600 mx-1 px-1"] do
        span_ do
            "Xterm ("
            (col, row) <- fromMaybe (0, 0) <$> lift (readTVar server.dimension)
            toHtml (show col)
            "x"
            toHtml (show row)
            ")"
        with span_ [class_ "border border-gray-500 ml-2 inline-flex rounded-md"] do
            with i_ [class_ "mx-1 cursor-pointer ri-fullscreen-line", onclick_ "resizeTerm()"] mempty
            with i_ [class_ "mx-1 cursor-pointer ri-subtract-line", onclick_ "hideTerm()"] mempty
            -- https://www.physics.udel.edu/~watson/scen103/ascii.html
            with pre_ [class_ "mx-1 cursor-pointer", onclick_ "socket.send('\\x0e')"] "C-n"

termApp :: Desktop -> App
termApp desktop =
    App
        { name = "term"
        , tags = fromList ["Development"]
        , description = "XTerm"
        , size = Nothing
        , triggers = fromList ["term-resize"]
        , start = const (startTermApp desktop)
        }

startTermApp :: Desktop -> WinID -> ProcessIO AppInstance
startTermApp desktop wid = do
    server <- atomically newXtermServer
    dimChan <- atomically newTChan

    let clientHandler _ _chan _client buf = atomically $ writeTChan server.inputChan buf
        dimHandler _ chan client buf = case (buf ^? key "cols" . _Integer, buf ^? key "rows" . _Integer) of
            (Just (unsafeFrom -> cols), Just (unsafeFrom -> rows)) -> do
                logInfo "Got resize" ["cols" .= cols, "rows" .= rows]
                atomically $ writeTChan dimChan (cols, rows)
                broadcastDesktopMessage desktop (\o -> o.endpoint /= client.endpoint) chan buf
            _ -> logError "invalid dim" ["buf" .= BSLog buf, "client" .= client]

    chan <- atomically (newHandler desktop.handlers clientHandler)
    dimChanID <- atomically (newHandler desktop.handlers dimHandler)

    let Workspace ws = desktop.workspace
        sess = from ("butler-" <> ws <> "-" <> showT chan)
    let (prog, args) = ("tmux", ["attach", "-t", sess])

    let mkProc =
            spawnProcess (ProgramName prog <> "-pty") do
                void $ System.Process.Typed.runProcess $ System.Process.Typed.proc "tmux" ["new-session", "-d", "-s", sess]
                logInfo "spawning" ["prog" .= prog]
                dim <- fromMaybe (80, 25) <$> atomically (readTVar server.dimension)
                (pty, phandle) <- liftIO (Pty.spawnWithPty Nothing True (from prog) args dim)
                handleProc pty `finally` do
                    liftIO $ cleanupProcess (Nothing, Nothing, Nothing, phandle)
        handleProc pty = do
            -- control handler
            spawnThread_ $ forever do
                dim <- atomically do
                    dim <- readTChan dimChan
                    writeTVar server.dimension (Just dim)
                    pure dim
                logTrace (prog <> "-resize") ["dim" .= dim]
                broadcastDraw desktop (pure . renderTray server)
                liftIO (Pty.resizePty pty dim)

            -- shell writer thread
            spawnThread_ $ forever do
                inputData <- atomically $ readTChan server.inputChan
                -- logTrace (prog <> "-write") ["buf" .= BSLog inputData]
                liftIO (Pty.writePty pty inputData)

            -- shell reader thread
            forever do
                outputData <- liftIO do
                    Pty.threadWaitReadPty pty
                    Pty.readPty pty
                -- logTrace (prog <> "-read") ["buf" .= BSLog outputData]
                broadcastDesktopMessage desktop (const True) chan outputData

    let welcomeMessage = "\rConnected to " <> encodeUtf8 prog
    statusMsg <- newTVarIO welcomeMessage

    let supervisor = do
            ptyProcess <- mkProc
            res <- awaitProcess ptyProcess
            logError "pty stopped" ["res" .= res]
            now <- getTime
            let errorMessage = "\r\n" <> from now <> " " <> encodeUtf8 prog <> " exited: " <> encodeUtf8 (from $ show res) <> "\r\n"
            atomically $ writeTVar statusMsg errorMessage
            broadcastDesktopMessage desktop (const True) chan errorMessage
            let waitForR = do
                    inputData <- atomically $ readTChan server.inputChan
                    unless (inputData == "r") waitForR
            waitForR
            supervisor

    let draw = pure . const (renderApp wid (from chan) (from dimChanID) server)
    -- tray = pure . renderTray server

    newAppInstance draw \events -> do
        spawnThread_ supervisor
        forever do
            ev <- atomically $ readPipe events
            logError "unknown ev" ["ev" .= ev]

termClient :: WinID -> Natural -> Natural -> Maybe (Int, Int) -> Text
termClient wid tid dimChanID dimM =
    [raw|
if (typeof butlerTerminals === "undefined") {
  globalThis.butlerTerminals = {}
}
globalThis.startClient = (wid, tid, dimChanID, w, h) => {
  // Start terminal
  var term = new Terminal({scrollback: 1e4});
  term.fitAddon = new FitAddon.FitAddon();
  term.loadAddon(term.fitAddon);
  term.open(document.getElementById('w-' + wid));
  term.writeln('Connecting...');
  butlerTerminals[tid] = term;

  // Handle i/o
  term.onData(d => {
    let ds = new TextEncoder().encode(d);
    let buf = new Uint8Array(1 + ds.length);
    buf[0] = tid
    buf.set(ds, 1)
    // console.log("Input to server:", tid, d, typeof d, buf)
    butlerDataSocket.send(buf)
  });

  butlerDataHandlers[tid] = buf => {term.write(buf)};
  butlerDataHandlers[dimChanID] = buf => {
    let body = decodeJSON(buf)
    console.log("Resizing from server", body)
    term.resize(body.cols, body.rows)
  }

  // Copy on selection
  term.onSelectionChange(e => {
    if (term.hasSelection()) {
      navigator.clipboard.writeText(term.getSelection())
   }
  })

  // Handle size
  term.onResize(e => {
    console.log("Resized", e);
    if (e.cols > 2 && e.rows > 2) {
      // somehow the viewport width doesn't match the screen, the next expression fix that.
      term._core._viewportElement.style.width = term._core.screenElement.style.width
    }
  });

  onWindowResize[wid] = (x, y) => {
    if (x === undefined) {
      console.log("Looking for term dim")
      term.resize(1, 2);
      term.fitAddon.fit()
      term.refresh(0, term.rows - 1)
      butlerDataSocket.send(encodeDataMessage(dimChanID, {cols: term.cols, rows: term.rows}))
    }
  }

  // TODO: Handle title
  term.onTitleChange(e => {
    console.log("Titled", e);
  })

  if (w !== undefined) {
    term.resize(w, h)
  }
}
|]
        <> "\n"
        <> case dimM of
            Just dim -> "startClient" <> from (show (wid, tid, dimChanID, fst dim, snd dim))
            Nothing -> "startClient" <> from (show (wid, tid, dimChanID))
