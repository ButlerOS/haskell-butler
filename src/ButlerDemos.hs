-- | Demos
module ButlerDemos where

import System.Environment (getArgs)
import System.Process.Typed hiding (Process)

import Butler
import Butler.App
import Butler.Core
import Butler.Display
import Butler.Lobby
import Butler.REPL
import Butler.UnixShell

import Butler.Auth.Invitation

import Butler.App.Chat
import Butler.App.Clock
import Butler.App.Desktop
import Butler.App.FileManager
import Butler.App.FileViewer
import Butler.App.JiraClient
import Butler.App.Launcher
import Butler.App.LogViewer
import Butler.App.MineSweeper
import Butler.App.Mumbler
import Butler.App.NoVnc
import Butler.App.Noter
import Butler.App.Painter
import Butler.App.PdfViewer
import Butler.App.PokerPlanner
import Butler.App.ProcessExplorer
import Butler.App.QRTest
import Butler.App.RandomCat
import Butler.App.SessionManager
import Butler.App.Settings
import Butler.App.SoundTest
import Butler.App.Tabletop
import Butler.App.Template
import Butler.App.Terminal
import Butler.App.Terminal.Emacs
import Butler.App.TodoManager

import Butler.Service.Assistant
import Butler.Service.Cursor
import Butler.Service.FileService
import Butler.Service.SoundBlaster
import Butler.Service.SshAgent

import Butler.App.SocialLogin
import Butler.Auth
import Butler.Auth.OIDC
import Lucid.XStatic
import XStatic.Butler as XStatic
import XStatic.Hyperscript qualified as XStatic
import XStatic.NoVNC qualified as XStatic
import XStatic.PcmPlayer qualified as XStatic
import XStatic.Remixicon qualified as XStatic
import XStatic.SweetAlert2 qualified as XStatic
import XStatic.Winbox qualified as XStatic
import XStatic.Xterm qualified as XStatic

-- | Demonstrate running external processes.
vncServer :: ProcessIO ()
vncServer = do
    let exts = ["MIT-SHM", "XTEST", "RANDR", "RENDER", "XFIXES", "DOUBLE-BUFFER", "DPMS", "GLX", "X-Resource", "XVideo"]
    runProc "xorg" $
        proc "xdummy" $
            ["-nolisten", "tcp"]
                <> concatMap (\e -> ["+extension", e]) exts
                <> ["+iglx", "-verbose", "9", "-logverbose", "9", ":99"]
                <> ["-config", "./xorg.conf"]

    sleep 2_000

    runProc "desktop" "env DISPLAY=:99 openbox-session"

    sleep 1_000
    runProc "clip" "env DISPLAY=:99 autocutsel -s CLIPBOARD -verbose"
    runProc_ "xrandr" "DISPLAY=:99 xrandr --output default --mode 800x600"
    runProc "xterm" "env DISPLAY=:99 xterm"
    runProc "vnc" "env DISPLAY=:99 x11vnc -noxdamage -noipv6 -alwaysshared -xrandr resize -ncache 10 -shared -forever"

    liftIO . print =<< waitProcess =<< getSelfProcess
  where
    runProc_ name cmd = void $ spawnProcess (ProgramName name) (runExternalProcess name cmd)
    runProc name cmd = void $ superviseProcess (ProgramName name) do
        runExternalProcess name cmd
        error "process died"

-- | Demonstrate apps deployment
demoApp :: ProcessIO Void
demoApp = serveApps (publicDisplayApp "Demo app" Nothing) [tabletopApp, qrApp, clockApp]

-- | Demonstrate dashboard apps deployment
demoDashboard :: ProcessIO Void
demoDashboard = serveDashboardApps (publicDisplayApp "Demo dashboard" Nothing) [clockApp, clockApp]

-- | Demonstrate a more complicated apps deployment with a lobby to dispatch client based on the path.
multiDesktop :: IO ()
multiDesktop = run (demoDesktop [])

-- | Demonstrate a social login app
demoSocialAuth :: IO ()
demoSocialAuth = do
    Just client_id <- liftIO $ getEnv "OIDC_ID"
    Just client_password <- liftIO $ getEnv "OIDC_SECRET"
    run $
        serveApps
            (publicOIDCDisplayApp (OIDCClientID client_id) (OIDCClientSecret client_password) "Demo social login app" Nothing)
            [socialLoginApp]

demoDesktop :: [App] -> ProcessIO Void
demoDesktop extraApps = withButlerSupervisor \butlerSupervisor -> do
    let authApp = invitationAuthApp indexHtml
    isolation <- getIsolation
    desktop <- superviseProcess "desktops" $ startDisplay Nothing allXfiles authApp $ \display -> do
        void $ superviseProcess "repl" (runUnixREPL (adminREPL display.sessions))
        chat <- atomically (newChatServer display.clients)
        lobbyProgram butlerSupervisor (mkAppSet chat butlerSupervisor isolation) chat display
    void $ waitProcess desktop
    error "oops"
  where
    indexHtml :: Html () -> Html ()
    indexHtml body = do
        doctypehtml_ do
            head_ do
                title_ "Butler"
                meta_ [charset_ "utf-8"]
                meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
                xstaticScripts xfiles
                link_ [rel_ "icon", href_ "/xstatic/favicon.ico"]
                -- fix the reconnect delay to 1sec
                script_ "htmx.config.wsReconnectDelay = (retryCount) => 1000;"

            with body_ [class_ "font-mono cursor-default bg-stone-100 h-screen"] do
                body
                -- Display an overlay when the connection is dropped
                script_ displayPulseError
                with div_ [class_ "h-full w-0 fixed z-50 top-0 left-0 bg-blue-300/[0.9] overflow-x-hidden", id_ "ws-error-overlay"] do
                    with div_ [class_ "flex justify-center items-center h-full"] do
                        with div_ [class_ "bg-blue-100 rounded p-2"] do
                            "Disconnected, retrying connection: "
                            with span_ [id_ "recon-count"] "0"

    displayPulseError :: Text
    displayPulseError =
        [raw|
    document.body.addEventListener('htmx:wsError', function(evt) {
      try {
        let elt = htmx.find("#ws-error-overlay")
        htmx.addClass(elt, "w-full");
        htmx.removeClass(elt, "w-0");
        let countElt = htmx.find("#recon-count")
        countElt.textContent = parseInt(countElt.textContent) + 1
      } catch (e) { console.log("Error", e); }
    });
    |]

    services butlerSupervisor isolation =
        [ soundBlasterService
        , cursorService
        , sshAgentService isolation
        , fileService
        , butlerService butlerSupervisor
        ]

    mkAppSet chat butlerSupervisor isolation =
        newAppSet $
            [ chatApp chat
            , clockApp
            , desktopApp (services butlerSupervisor isolation)
            , logViewerApp
            , termApp isolation
            , emacsApp isolation
            , soundTestApp
            , mumblerApp
            , peApp
            , vncApp
            , qrApp
            , smApp
            , tabletopApp
            , mineSweeperApp
            , fileManagerApp
            , fileViewerApp
            , launcherApp
            , noterApp
            , painterApp
            , pdfViewerApp
            , pokerPlannerApp
            , randomCatApp
            , todoManagerApp
            , settingsApp
            , templateApp
            , jiraClientApp
            ]
                <> extraApps

    allXfiles, xfiles :: [XStaticFile]
    xfiles =
        defaultXFiles
            <> [ XStatic.sweetAlert2
               , XStatic.remixiconCss
               , XStatic.remixiconWoff2
               , XStatic.logo
               , XStatic.xtermFitAddonJs
               , XStatic.xtermFitAddonJsMap
               , XStatic.xtermWebGLAddonJs
               , XStatic.xtermWebGLAddonJsMap
               , XStatic.winboxCss
               , XStatic.hyperscript
               ]
            <> XStatic.xterm
            <> XStatic.pcmPlayer
            <> noterApp.xfiles
            <> pdfViewerApp.xfiles
    allXfiles = pdfViewerApp.extraXfiles <> noterApp.extraXfiles <> XStatic.noVNC <> XStatic.winbox <> xfiles

run :: ProcessIO _ -> IO ()
run action = withButlerOS action >>= print

replClient :: [String] -> ProcessIO ()
replClient xs = withUnixSocket "control.sock" \socket -> do
    sktSendAll socket (from $ encodeJSON (into @Text <$> xs))
    res <- sktRecv socket 4096
    liftIO $ putTextLn $ decodeUtf8 res

main :: IO ()
main = runMain do
    getArgs >>= \case
        ["vnc"] -> run vncServer
        ["app"] -> run demoApp
        ["dashboard"] -> run demoDashboard
        "run" : xs -> run $ replClient xs
        _ -> run (demoDesktop [])
