module Butler.App.Terminal (termApp) where

import Butler.Prelude

import System.Posix.Pty qualified as Pty

import Butler
import Butler.Frame

import System.Process (cleanupProcess)
import System.Process.Typed qualified

data XtermServer = XtermServer
    { inputChan :: TChan ByteString
    , dimension :: TVar (Maybe (Int, Int))
    }

newXtermServer :: STM XtermServer
newXtermServer = XtermServer <$> newTChan <*> newTVar Nothing

renderApp :: WinID -> XtermServer -> HtmlT STM ()
renderApp wid server =
    with div_ [wid_ wid "w", class_ "w-full h-full border-2 border-indigo-600"] do
        mDim <- lift (readTVar server.dimension)
        script_ (termClient wid mDim)

renderTray :: WinID -> XtermServer -> HtmlT STM ()
renderTray wid server = do
    with span_ [wid_ wid "bar", class_ "inline-block bg-stone-600 mx-1 px-1"] do
        span_ do
            "XTerm ("
            (col, row) <- fromMaybe (0, 0) <$> lift (readTVar server.dimension)
            toHtml (show col)
            "x"
            toHtml (show row)
            ")"
        with span_ [class_ "border border-gray-500 ml-2 inline-flex rounded-md"] do
            with i_ [class_ "mx-1 cursor-pointer ri-fullscreen-line", onclick_ ("onWindowResize[" <> showT wid <> "]()")] mempty
            -- https://www.physics.udel.edu/~watson/scen103/ascii.html
            with pre_ [class_ "mx-1 cursor-pointer", onclick_ (termCmd "butlerForward('\\x0e')")] "C-n"
            with (script_ mempty) [wid_ wid "script"]
  where
    termCmd cmd = "butlerTerminals[" <> showT wid <> "]." <> cmd

termApp :: Text -> App
termApp name =
    App
        { name = "term"
        , tags = fromList ["Development"]
        , description = "XTerm"
        , size = Nothing
        , start = startTermApp name
        }

startTermApp :: Text -> AppStart
startTermApp name clients wid pipeAE = do
    server <- atomically newXtermServer

    let draw :: HtmlT STM ()
        draw = do
            renderApp wid server
            renderTray wid server

    spawnThread_ $ forever do
        ev <- atomically (readPipe pipeAE)
        case ev of
            AppDisplay _ -> sendHtmlOnConnect draw ev
            AppData de -> atomically $ writeTChan server.inputChan de.buffer
            AppTrigger de ->
                case (de.body ^? key "cols" . _Integer, de.body ^? key "rows" . _Integer) of
                    (Just (unsafeFrom -> cols), Just (unsafeFrom -> rows)) -> do
                        logInfo "Got resize" ["cols" .= cols, "rows" .= rows]
                        atomically $ writeTVar server.dimension (Just (cols, rows))
                        let resizeScript = "butlerTerminals[" <> showT wid <> "].butlerResize" <> showT (cols, rows)
                        sendsHtmlButSelf de.client clients do
                            with (script_ resizeScript) [wid_ wid "script"]
                    _ -> logError "invalid dim" ["ev" .= de]

    let sess = from ("butler-" <> name <> "-" <> showT wid)
    let (prog, args) = ("tmux", ["attach", "-t", sess])

    let mkProc =
            spawnProcess (ProgramName prog <> "-pty") do
                void $ System.Process.Typed.runProcess $ System.Process.Typed.proc "tmux" ["new-session", "-d", "-s", sess, "env", "-", "TERM=xterm", "bash"]
                logInfo "spawning" ["prog" .= prog]
                dim <- fromMaybe (80, 25) <$> readTVarIO server.dimension
                (pty, phandle) <- liftIO (Pty.spawnWithPty Nothing True (from prog) args dim)
                handleProc dim pty `finally` do
                    liftIO do
                        Pty.closePty pty
                        cleanupProcess (Nothing, Nothing, Nothing, phandle)
        handleProc startDim pty = do
            -- control handler
            spawnThread_ do
                let handleDimChange dim = do
                        mNewDim <- atomically do
                            currentDim <- readTVar server.dimension
                            if currentDim == dim
                                then retrySTM
                                else pure currentDim
                        logTrace (prog <> " term resized") ["dim" .= dim]
                        forM_ mNewDim \newDim -> do
                            sendsHtml clients (renderTray wid server)
                            liftIO (Pty.resizePty pty newDim)
                        handleDimChange mNewDim
                handleDimChange (Just startDim)
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
                sendsBinary clients (encodeMessageL wid (from outputData))

    let welcomeMessage = "\rConnected to " <> encodeUtf8 prog
    statusMsg <- newTVarIO welcomeMessage

    let supervisor = do
            ptyProcess <- mkProc
            res <- awaitProcess ptyProcess
            logError "pty stopped" ["res" .= res]
            now <- getTime
            let errorMessage = "\r\n" <> from now <> " " <> encodeUtf8 prog <> " exited: " <> encodeUtf8 (from $ show res) <> "\r\n"
            atomically $ writeTVar statusMsg errorMessage
            sendsBinary clients (encodeMessageL wid (from errorMessage))

            let waitForR = do
                    inputData <- atomically $ readTChan server.inputChan
                    unless (inputData == "r") waitForR
            waitForR
            supervisor

    supervisor

termClient :: WinID -> Maybe (Int, Int) -> Text
termClient wid mDim =
    [raw|
if (typeof butlerTerminals === "undefined") {
  globalThis.butlerTerminals = {}
}
function startTermClient(wid, w, h) {
  // Start terminal
  var term = new Terminal({scrollback: 1e4});
  term.fitAddon = new FitAddon.FitAddon();
  term.loadAddon(term.fitAddon);
  term.open(document.getElementById('w-' + wid));
  term.writeln('Connecting...');
  butlerTerminals[wid] = term;

  // Handle i/o
  term.butlerForward = d => {
    let ds = new TextEncoder().encode(d);
    let buf = new Uint8Array(1 + ds.length);
    buf[0] = wid
    buf.set(ds, 1)
    // console.log("Input to server:", tid, buf)
    butlerDataSocket.send(buf)
  }
  term.onData(d => {
    term.butlerForward(d)
  });

  butlerDataHandlers[wid] = buf => {term.write(buf)};

  term.butlerResize = (cols, rows) => {
    term.resize(cols, rows)
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
      term.refresh(0, term.rows)
      sendTrigger(wid, "resize", {cols: term.cols, rows: term.rows})
    }
  }

  // TODO: Handle title
  term.onTitleChange(e => {
    console.log("Titled", e);
  })

  if (w !== undefined) {
    term.resize(w, h)
  } else {
    setTimeout(() => onWindowResize[wid](), 500)
  }
}
|]
        <> "\n"
        <> case mDim of
            Just dim -> "startTermClient" <> showT (wid, fst dim, snd dim)
            Nothing -> "startTermClient(" <> showT wid <> ")"
