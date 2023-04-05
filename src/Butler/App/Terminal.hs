module Butler.App.Terminal (termApp) where

import Data.List qualified
import System.Posix.Files qualified
import System.Posix.Pty qualified as Pty

import Butler
import Butler.Frame
import Butler.UnixShell
import XStatic.Xterm qualified as XStatic

import System.Process (cleanupProcess)
import System.Process.Typed qualified

data XtermServer = XtermServer
    { inputChan :: TChan ByteString
    , dimension :: TVar (Maybe (Int, Int))
    }

newXtermServer :: STM XtermServer
newXtermServer = XtermServer <$> newTChan <*> newTVar Nothing

renderApp :: AppID -> XtermServer -> HtmlT STM ()
renderApp wid server =
    with div_ [wid_ wid "w", class_ "w-full h-full border-2 border-indigo-600"] do
        mDim <- lift (readTVar server.dimension)
        script_ (termClient wid mDim)

renderTray :: AppID -> XtermServer -> HtmlT STM ()
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

termApp :: Isolation -> App
termApp isolation =
    (defaultApp "term" (startTermApp isolation))
        { tags = fromList ["Development"]
        , description = "XTerm"
        , xfiles = [XStatic.xtermFitAddonJs, XStatic.xtermFitAddonJsMap] <> XStatic.xterm
        }

startTermApp :: Isolation -> AppContext -> ProcessIO ()
startTermApp isolation ctx = do
    let clients = ctx.shared.clients
        wid = ctx.wid
    server <- atomically newXtermServer

    let draw :: HtmlT STM ()
        draw = do
            renderApp wid server
            renderTray wid server

    spawnThread_ $ forever do
        ev <- atomically (readPipe ctx.pipe)
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
            _ -> logError "Unexpected event" ["ev" .= ev]

    -- prep directory tree
    baseDir <- from . decodeUtf8 <$> getPath "rootfs"
    let sktPath = baseDir </> "skt"
        homePath = baseDir </> "home"
    case isolation.runtime of
        None -> pure ()
        _ -> do
            liftIO $ createDirectoryIfMissing True sktPath
            liftIO $ createDirectoryIfMissing True homePath

    -- Setup env
    let pathEnv = do
            path <- isolation.toolbox
            pure $ "PATH=" <> path </> "bin:/bin:/sbin"
        agentEnv =
            "SSH_AUTH_SOCK=" <> case isolation.runtime of
                None -> "/tmp/butler-agent.sock"
                _ -> "/butler/skt/agent.sock"
    homeEnv <-
        ("HOME=" <>) <$> case isolation.runtime of
            None -> maybe "/tmp" (into @String . decodeUtf8) <$> liftIO (getEnv "HOME")
            _ -> pure "/butler/home"

    addResolv <- case isolation.runtime of
        Bubblewrap -> liftIO do
            isSymlink <- System.Posix.Files.isSymbolicLink <$> System.Posix.Files.getSymbolicLinkStatus "/etc/resolv.conf"
            if isSymlink
                then do
                    realResolvConf <- do
                        realResolvConf <- System.Posix.Files.readSymbolicLink "/etc/resolv.conf"
                        pure $
                            if "../" `Data.List.isPrefixOf` realResolvConf
                                then drop 2 realResolvConf
                                else realResolvConf
                    pure (realResolvConf :)
                else pure id
        _ -> pure id

    let tmuxPath = case isolation.toolbox of
            Just path -> path </> "bin/tmux"
            Nothing -> "tmux"

    let sess = "butler-term-" <> show wid
        tmuxAttach = ["-2", "attach", "-t", sess] :: [String]
        env = maybe id (:) pathEnv [homeEnv, agentEnv, "TERM=xterm-256color"]
        cmd = "env" : "-" : env <> ["bash", "-l"]
        tmuxSession = ["-2", "new-session", "-d", "-s", sess] <> cmd
        prog = case isolation.runtime of
            None -> into @Text tmuxPath
            Bubblewrap{} -> "bwrap"
            Podman{} -> "podman"
        mkArgs dieWithParent baseArgs = case isolation.runtime of
            None -> baseArgs
            Podman image -> ["run", via @Text image, "--", "tmux"] <> baseArgs
            Bubblewrap ->
                let addNix = case isolation.toolbox of
                        Nothing -> id
                        Just{} -> \xs -> "--bind" : "/nix" : "/nix" : xs
                    addEnv = case isolation.toolbox of
                        Nothing -> id
                        Just path -> \xs -> "--setenv" : "PATH" : (path </> "bin:/bin:/sbin") : xs
                    addDieWithParent
                        | dieWithParent = ("--die-with-parent" :)
                        | otherwise = id
                 in addNix . addEnv . addDieWithParent $
                        ["--unshare-pid", "--unshare-ipc", "--unshare-uts"]
                            <> ["--bind", baseDir, "/butler", "--chdir", "/butler"]
                            <> concatMap (\p -> ["--ro-bind", p, p]) (addResolv ["/usr", "/lib", "/lib64", "/bin", "/sbin", "/etc", "/sys"])
                            <> ["--proc", "/proc", "--dev", "/dev", "--perms", "01777", "--tmpfs", "/tmp", "--tmpfs", "/dev/shm", "--tmpfs", "/run/user"]
                            <> ["--tmpfs", "/etc/ssh/ssh_config.d"] -- remove host default because ssh fails with `Bad owner or permissions`
                            <> (tmuxPath : "-S" : "/butler/skt/default" : baseArgs)

        mkProc =
            spawnProcess (ProgramName prog <> "-pty") do
                let procSpec = System.Process.Typed.proc (from prog) (mkArgs False tmuxSession)
                logDebug "spawning" ["prog" .= show procSpec]
                void $ System.Process.Typed.runProcess procSpec
                dim <- fromMaybe (80, 25) <$> readTVarIO server.dimension
                (pty, phandle) <- liftIO (Pty.spawnWithPty Nothing True (from prog) (mkArgs True tmuxAttach) dim)
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
                        logDebug (prog <> " term resized") ["dim" .= dim]
                        forM_ mNewDim \newDim -> do
                            sendsHtml clients (renderTray wid server)
                            liftIO (Pty.resizePty pty newDim)
                        handleDimChange mNewDim
                handleDimChange (Just startDim)
            -- shell writer thread
            spawnThread_ $ forever do
                inputData <- atomically $ readTChan server.inputChan
                -- logDebug (prog <> "-write") ["buf" .= BSLog inputData]
                liftIO (Pty.writePty pty inputData)

            -- shell reader thread
            forever do
                outputData <- liftIO do
                    Pty.threadWaitReadPty pty
                    Pty.readPty pty
                -- logDebug (prog <> "-read") ["buf" .= BSLog outputData]
                sendsBinary clients (encodeMessage (from wid) (from outputData))

    let welcomeMessage = "\rConnected to " <> encodeUtf8 prog
    statusMsg <- newTVarIO welcomeMessage

    let supervisor = do
            ptyProcess <- mkProc
            res <- waitProcess ptyProcess
            logError "pty stopped" ["res" .= res]
            now <- getTime
            let errorMessage = "\r\n" <> from now <> " " <> encodeUtf8 prog <> " exited: " <> encodeUtf8 (from $ show res) <> "\r\n"
            atomically $ writeTVar statusMsg errorMessage
            sendsBinary clients (encodeMessage (from wid) (from errorMessage))

            let waitForR = do
                    inputData <- atomically $ readTChan server.inputChan
                    unless (inputData == "r") waitForR
            waitForR
            supervisor

    supervisor

termClient :: AppID -> Maybe (Int, Int) -> Text
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
