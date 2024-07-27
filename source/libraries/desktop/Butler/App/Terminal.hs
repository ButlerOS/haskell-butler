module Butler.App.Terminal (termApp, replApp, TermApp (..), startTermApp) where

import Data.ByteString qualified as BS
import Data.List qualified
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import System.Directory (doesPathExist)
import System.FilePath (takeDirectory)
import System.Posix.Files qualified
import System.Posix.Pty qualified as Pty

import Butler
import Butler.App
import Butler.Frame
import Butler.UnixShell
import XStatic.Xterm qualified as XStatic

import System.Posix.Pty (TerminalMode (EnableEcho), TerminalState (..), getTerminalAttributes, setTerminalAttributes, withoutMode)
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
    (defaultApp "term" (startTermApp isolation (Just tmuxTermApp)))
        { tags = fromList ["Development"]
        , description = "XTerm"
        , xfiles = XStatic.xtermWebGLAddonJs : XStatic.xtermWebGLAddonJsMap : XStatic.xtermFitAddonJs : XStatic.xtermFitAddonJsMap : XStatic.xterm
        }

replApp :: Isolation -> App
replApp isolation = defaultApp "termREPL" (startTermApp isolation Nothing)

data TermApp = TermApp
    { backgroundCommand :: Maybe [String]
    , attachCommand :: [String]
    }

tmuxTermApp :: Isolation -> String -> TermApp
tmuxTermApp isolation name =
    TermApp
        { backgroundCommand = Just $ cmd : withSktPath ["-2", "new-session", "-d", "-s", name, "bash", "-l"]
        , attachCommand = cmd : withSktPath ["-2", "attach", "-t", name]
        }
  where
    cmd = case isolation.toolbox of
        Just path -> path </> "bin/tmux"
        Nothing -> "tmux"
    withSktPath = case isolation.runtime of
        None -> id
        _ -> \xs -> "-S" : "/butler/.skt/default" : xs

writeIfNeeded :: MonadIO m => FilePath -> Text -> m ()
writeIfNeeded fp content = liftIO do
    pathExist <- doesPathExist fp
    if pathExist
        then do
            prev <- Text.readFile fp
            when (prev /= content) do
                Text.writeFile fp content
        else do
            createDirectoryIfMissing True (takeDirectory fp)
            Text.writeFile fp content

startTermApp :: Isolation -> Maybe (Isolation -> String -> TermApp) -> AppContext -> ProcessIO ()
startTermApp isolation mMkApp ctx = do
    let clients = ctx.shared.clients
        wid = ctx.wid
    server <- atomically newXtermServer

    let draw :: HtmlT STM ()
        draw = do
            renderApp wid server
            renderTray wid server

    (vInput :: MVar ByteString) <- newEmptyMVar
    vApp <- newEmptyMVar
    spawnThread_ $ forever do
        ev <- atomically (readPipe ctx.pipe)
        case ev of
            AppDisplay _ -> sendHtmlOnConnect draw ev
            AppData de -> atomically $ writeTChan server.inputChan (from de.buffer)
            AppSync es -> case es.name of
                "start-repl" | Just args <- fromDynamic @[String] es.message -> do
                    -- Purge existing app definition
                    void $ tryTakeMVar vApp
                    let
                        mkApp :: Isolation -> String -> TermApp
                        mkApp _ _ = TermApp Nothing args
                    putMVar vApp mkApp
                    logInfo "Acknowledge the start-repl event" []
                    atomically (putTMVar es.reply (toDyn ()))
                "new-doc" | Just args <- fromDynamic @Text es.message -> do
                    putMVar vInput $ encodeUtf8 args
                _ -> logError "Bad dynamics" ["action" .= es.name]
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

    mkApp <- case mMkApp of
        Nothing -> takeMVar vApp
        Just v -> pure v
    let replMode = isNothing mMkApp

    -- prep directory tree
    baseDir <- from . decodeUtf8 <$> getPath "rootfs"
    let sktPath = baseDir </> ".skt"
        homePath = baseDir </> "home"
    case isolation.runtime of
        None -> pure ()
        _ -> do
            liftIO $ createDirectoryIfMissing True sktPath
            liftIO $ createDirectoryIfMissing True homePath

    -- Setup env
    let pathEnv = do
            path <- isolation.toolbox
            pure $ "PATH=" <> path </> "bin:/bin:/sbin:/butler/home/.nix-profile/bin"
        agentEnv =
            "SSH_AUTH_SOCK=" <> case isolation.runtime of
                None -> "/tmp/butler-agent.sock"
                _ -> "/butler/.skt/agent.sock"
    homeEnv <-
        ("HOME=" <>) <$> case isolation.runtime of
            None -> maybe "/tmp" (into @String . decodeUtf8) <$> liftIO (getEnv "HOME")
            _ -> pure "/butler/home"

    let tlsBundleFile = "/etc/pki/tls/certs/ca-bundle.crt"
    tlsEnv <-
        liftIO (doesPathExist tlsBundleFile) >>= \case
            False -> pure Nothing
            True -> pure (Just $ "NIX_SSL_CERT_FILE=" <> tlsBundleFile)

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

    fixSshConfig <- case isolation.runtime of
        Bubblewrap -> liftIO do
            let sshConfigPath = "/etc/ssh/ssh_config.d"
            -- With bwrap, that directory is owned by nobody and ssh fails with `Bad owner or permissions`
            doesPathExist sshConfigPath >>= \case
                True -> pure (\xs -> "--tmpfs" : "/etc/ssh/ssh_config.d" : xs)
                False -> pure id
        _ -> pure id

    fixSshHome <- do
        -- the ssh command reads the home path from /etc/passwd, thus we need to make the bwrap home match.
        -- TODO: resolve real home
        let realHome = "/home/fedora"
        pure $ \xs -> "--symlink" : "/butler/home" : realHome : xs

    case isolation.runtime of
        None -> pure ()
        _ -> do
            let nixConfigHostPath = homePath </> ".config/nix/nix.conf"
            writeIfNeeded nixConfigHostPath $
                Text.unlines ["sandbox = false", "build-users-group =", "experimental-features = nix-command flakes"]

    let tApp = mkApp isolation $ "butler-term-" <> show wid
        env = "-" : maybe id (:) tlsEnv (maybe id (:) pathEnv [homeEnv, agentEnv, "TERM=xterm-256color", "LC_ALL=C.UTF-8"])
        mkArgs isBackground baseArgs = case isolation.runtime of
            None -> baseArgs
            Podman image -> "podman" : "run" : via @Text image : "--" : baseArgs
            Bubblewrap ->
                let addNix = case isolation.toolbox of
                        Nothing -> id
                        Just{} -> \xs -> "--bind" : "/nix" : "/nix" : xs
                    addDieWithParent
                        | isBackground = ("--die-with-parent" :)
                        | otherwise = id
                    bwrapArgs =
                        addNix . addDieWithParent $
                            ["--unshare-pid", "--unshare-ipc", "--unshare-uts"]
                                <> ["--bind", baseDir, "/butler", "--chdir", "/butler"]
                                <> concatMap (\p -> ["--ro-bind", p, p]) (addResolv ["/usr", "/lib", "/lib64", "/bin", "/sbin", "/etc", "/sys"])
                                <> fixSshConfig ["--proc", "/proc", "--dev", "/dev", "--perms", "01777", "--tmpfs", "/tmp", "--tmpfs", "/dev/shm", "--tmpfs", "/run/user"]
                                <> fixSshHome baseArgs
                 in "bwrap" : bwrapArgs
        attachArgs = env <> mkArgs True tApp.attachCommand
        mkProc clearScreen =
            spawnProcess (ProgramName $ showT ctx.wid <> "-pty") do
                forM_ tApp.backgroundCommand \bgCommand -> do
                    let procSpec = System.Process.Typed.proc "env" (env <> mkArgs False bgCommand)
                    logDebug "spawning" ["prog" .= show procSpec]
                    void $ System.Process.Typed.runProcess procSpec
                dim <- fromMaybe (80, 25) <$> readTVarIO server.dimension
                unless replMode do
                    logDebug "attaching" ["args" .= attachArgs]
                (pty, phandle) <- liftIO do
                    Pty.spawnWithPty Nothing True "env" attachArgs dim
                when replMode $ liftIO do
                    -- Disable echo in REPL mode
                    termAttrs <- liftIO (getTerminalAttributes pty)
                    setTerminalAttributes pty (withoutMode termAttrs EnableEcho) Immediately
                handleProc clearScreen dim pty `finally` do
                    liftIO do
                        Pty.closePty pty
                        cleanupProcess (Nothing, Nothing, Nothing, phandle)

        handleProc clearScreen startDim pty = do
            -- control handler
            spawnThread_ do
                let handleDimChange dim = do
                        mNewDim <- atomically do
                            currentDim <- readTVar server.dimension
                            if currentDim == dim
                                then retrySTM
                                else pure currentDim
                        logDebug "term resized" ["dim" .= dim]
                        forM_ mNewDim \newDim -> do
                            sendsHtml clients (renderTray wid server)
                            liftIO (Pty.resizePty pty newDim)
                        handleDimChange mNewDim
                handleDimChange (Just startDim)

            -- shell writer thread
            spawnThread_ $ forever do
                inputData <- atomically $ readTChan server.inputChan
                liftIO (Pty.writePty pty inputData)

            -- shell reader thread
            handleProcOutput clearScreen pty

        handleProcOutput clearScreen pty = do
            when clearScreen do
                sleep 80
            outputData <- liftIO do
                Pty.threadWaitReadPty pty
                Pty.readPty pty
            let buffer
                    | clearScreen = "\x1b[2J\x1b[H" <> from outputData
                    | otherwise = from outputData
            sendsBinary clients (encodeMessage (from wid) buffer)
            handleProcOutput False pty

    let welcomeMessage = "\rConnected!"
    statusMsg <- newTVarIO welcomeMessage

    let
        doWaitREPL ptyProcess = do
            logInfo "Waiting for vInput" []
            input <- takeMVar vInput
            atomically $ writeTChan server.inputChan input
            unless ("\n" `BS.isSuffixOf` input) do
                atomically $ writeTChan server.inputChan "\n"
            -- Wait for a new input, but keep it in the MVar for the next loop
            void $ readMVar vInput
            -- A new input arrived, restart the process
            void $ atomically $ stopProcess ptyProcess
            void $ waitProcess ptyProcess
        doWaitProcess ptyProcess = do
            res <- waitProcess ptyProcess
            logError "pty stopped" ["res" .= res]
            now <- getTime
            let errorMessage = "\r\n" <> from now <> " exited: " <> encodeUtf8 (from $ show res) <> "\r\n"
            atomically $ writeTVar statusMsg errorMessage
            sendsBinary clients (encodeMessage (from wid) (from errorMessage))

            let waitForR = do
                    inputData <- atomically $ readTChan server.inputChan
                    unless (inputData == "r") waitForR
            waitForR

    forever do
        ptyProcess <- mkProc replMode
        if replMode
            then doWaitREPL ptyProcess
            else doWaitProcess ptyProcess

termClient :: AppID -> Maybe (Int, Int) -> Text
termClient wid mDim =
    [raw|
if (typeof butlerTerminals === "undefined") {
  globalThis.butlerTerminals = {}
}
function startTermClient(wid, w, h) {
  // Start terminal
  var term = new Terminal({scrollback: 1e4});

  // fit-addon
  const fitAddon = new FitAddon.FitAddon();
  term.loadAddon(fitAddon);

  // webgl-addon
  const webGLAddon = new window["WebglAddon"].WebglAddon()
  webGLAddon.onContextLoss(e => {
    console.log("Context lost!")
    webGLAddon.dispose();
  });
  term.loadAddon(webGLAddon)

  term.open(document.getElementById('w-' + wid));
  term.writeln('Connecting...');
  butlerTerminals[wid] = term;

  // Handle i/o
  term.butlerForward = d => {
    let ds = new TextEncoder().encode(d);
    sendBinaryMessage(wid, ds)
  }
  term.onData(d => {
    term.butlerForward(d)
  });

  butlerDataHandlers[wid] = buf => {
    term.write(buf)
  };

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
      fitAddon.fit()
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
