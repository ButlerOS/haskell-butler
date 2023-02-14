-- | Demos
module ButlerDemos where

import Control.Concurrent.CGroup qualified
import Main.Utf8
import System.Environment (getArgs)
import System.Process.Typed hiding (Process)

import Butler.App
import Butler.Desktop
import Butler.Display
import Butler.GUI
import Butler.Lobby
import Butler.Logger
import Butler.Prelude

import Butler.Auth.Guest
import Butler.Auth.Invitation

import Butler.App.Chat
import Butler.App.Clock
import Butler.App.LogViewer
import Butler.App.MineSweeper
import Butler.App.Mumbler
import Butler.App.NoVnc
import Butler.App.ProcessExplorer
import Butler.App.Seat
import Butler.App.SessionManager
import Butler.App.SoundTest
import Butler.App.Tabletop
import Butler.App.Terminal
import Butler.Clock

import Butler.Session (Sessions)
import Lucid.XStatic
import XStatic.Butler as XStatic
import XStatic.Htmx qualified as XStatic
import XStatic.NoVNC qualified as XStatic
import XStatic.PcmPlayer qualified as XStatic
import XStatic.Remixicon qualified as XStatic
import XStatic.SweetAlert2 qualified as XStatic
import XStatic.Tailwind qualified as XStatic
import XStatic.Winbox qualified as XStatic
import XStatic.Xterm qualified as XStatic

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
    runProc' "xrandr" "DISPLAY=:99 xrandr --output default --mode 800x600"
    runProc "xterm" "env DISPLAY=:99 xterm"
    runProc "vnc" "env DISPLAY=:99 x11vnc -noxdamage -noipv6 -alwaysshared -xrandr resize -ncache 10 -shared -forever"

    liftIO . print =<< awaitProcess =<< getSelfProcess
  where
    runProc' name cmd = void $ spawnProcess (ProgramName name) (runExternalProcess name cmd)
    runProc name cmd = void $ superviseProcess (ProgramName name) do
        runExternalProcess name cmd
        error "process died"

standaloneGuiApp :: ProcessIO ()
standaloneGuiApp = serveAppPerClient defaultXFiles auth mineSweeperApp
  where
    auth = const . pure . guestAuthApp $ htmlMain defaultXFiles "Standalone GUI" Nothing

htmlMain :: [XStaticFile] -> Text -> Maybe (Html ()) -> Html ()
htmlMain xfiles title mHtml = do
    doctypehtml_ do
        head_ do
            title_ (toHtml title)
            meta_ [charset_ "utf-8"]
            meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
            xstaticScripts xfiles

        with body_ [class_ "font-mono cursor-default bg-stone-100 h-screen"] do
            with div_ [id_ "display-ws", class_ "h-full", makeAttribute "hx-ext" "ws", makeAttribute "ws-connect" "/ws/htmx"] do
                with div_ [id_ "w-0", class_ "h-full"] mempty
                forM_ mHtml id

serveAppPerClient :: [XStaticFile] -> (Sessions -> ProcessIO AuthApplication) -> App -> ProcessIO ()
serveAppPerClient xfiles mkAuth app = do
    desktop <- superviseProcess "gui" $
        startDisplay 8085 xfiles mkAuth $ \_ -> do
            pure $ \_ws -> do
                env <- ask
                -- The list of clients and the app instance is re-created per client
                clients <- atomically newDisplayClients
                appInstance <- startApp app clients (WinID 0)
                pure (env, clientHandler clients appInstance)

    void $ awaitProcess desktop
  where
    clientHandler :: DisplayClients -> AppInstance -> DisplayEvent -> ProcessIO ()
    clientHandler clients appInstance displayEvent = case displayEvent of
        UserConnected "htmx" client -> do
            logInfo "Client connected" ["client" .= client]
            spawnThread_ (pingThread client)
            spawnThread_ (sendThread client)
            atomically do
                addClient clients client
            writePipe appInstance.pipeAE (AppDisplay displayEvent)
            forever do
                dataMessage <- recvData client
                case eventFromMessage client dataMessage of
                    Nothing -> logError "Unknown data" ["ev" .= LBSLog (into @LByteString dataMessage)]
                    Just (_wid, ae) -> writePipe appInstance.pipeAE ae
        UserDisconnected "htmx" client -> do
            logInfo "Client disconnected" ["client" .= client]
            atomically do
                addClient clients client
            writePipe appInstance.pipeAE (AppDisplay displayEvent)
            void $ killProcess appInstance.process.pid
        _ -> logError "Unknown event" ["ev" .= displayEvent]

defaultXFiles :: [XStaticFile]
defaultXFiles = [XStatic.htmx, XStatic.butlerWS, XStatic.tailwind]

multiDesktop :: IO ()
multiDesktop = do
    v <- runDemo
    putStrLn $ "The end: " <> show v
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
                            "Disconnected, retrying connection..."

    displayPulseError :: Text
    displayPulseError =
        [raw|
    document.body.addEventListener('htmx:wsError', function(evt) {
      try {
        let elt = htmx.find("#ws-error-overlay");
        htmx.addClass(elt, "w-full");
        htmx.removeClass(elt, "w-0");
      } catch (e) { console.log("Error", e); }
    });
    |]

    runDemo =
        withButlerOS do
            let authApp = invitationAuthApp indexHtml
            desktop <- superviseProcess "desktop" $ startDisplay 8080 xfiles' authApp $ \display -> do
                chat <- atomically (newChatServer display.clients)
                lobbyProgram (mkAppSet chat) xinit chat display
            void $ awaitProcess desktop

    xinit desktop = do
        let seatApp' = seatApp desktop.soundCard
        atomically . addDesktopApp desktop =<< startApp seatApp' desktop.clients (WinID 2)

    mkAppSet chat desktop =
        newAppSet
            [ chatApp chat
            , clockApp
            , logViewerApp
            , termApp "xterm"
            , soundTestApp desktop.soundCard
            , mumblerApp desktop.soundCard
            , peApp desktop
            , vncApp
            , smApp desktop.display
            , tabletopApp
            , mineSweeperApp
            ]

    xfiles', xfiles :: [XStaticFile]
    xfiles =
        defaultXFiles
            <> [ XStatic.sweetAlert2
               , XStatic.remixiconCss
               , XStatic.remixiconWoff2
               , XStatic.logo
               , XStatic.xtermFitAddonJs
               , XStatic.xtermFitAddonJsMap
               , XStatic.winboxCss
               ]
            <> XStatic.xterm
            <> XStatic.pcmPlayer
    xfiles' = XStatic.noVNC <> XStatic.winbox <> xfiles

run :: ProcessIO _ -> IO ()
run action = withButlerOS action >>= print

main :: IO ()
main = Main.Utf8.withUtf8 do
    -- ensure tty-less environment gets line based output
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering
    -- make rts aware of the cgroup capabilities
    Control.Concurrent.CGroup.initRTSThreads
    getArgs >>= \case
        ["vnc"] -> run vncServer
        ["ms"] -> run standaloneGuiApp
        _ -> multiDesktop
