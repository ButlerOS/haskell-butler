{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Butler where

import Control.Concurrent.CGroup qualified
import Data.ByteString qualified as BS
import Data.Text qualified as Text
import Main.Utf8
import System.Environment (getArgs)
import System.Process.Typed hiding (Process)

import Butler.Desktop
import Butler.Display
import Butler.GUI
import Butler.Lobby
import Butler.OS
import Butler.Prelude

import Butler.Auth.Guest
import Butler.Auth.Invitation

import Butler.App.Chat
import Butler.App.Clock
import Butler.App.LogViewer
import Butler.App.MineSweeper (mineSweeperApp)
import Butler.App.NoVnc
import Butler.App.ProcessExplorer
import Butler.App.Seat
import Butler.App.SessionManager
import Butler.App.SpeedTest
import Butler.App.Tabletop
import Butler.App.Terminal

import Butler.Clock
import Butler.Logger
import Butler.Window

import Lucid.XStatic
import XStatic.Butler as XStatic
import XStatic.Htmx qualified as XStatic
import XStatic.Hyperscript qualified as XStatic
import XStatic.NoVNC qualified as XStatic
import XStatic.Remixicon qualified as XStatic
import XStatic.SweetAlert2 qualified as XStatic
import XStatic.Tailwind qualified as XStatic
import XStatic.Winbox qualified as XStatic
import XStatic.Xterm qualified as XStatic

import Lucid
import Lucid.Base (makeAttribute)

startVnc :: ProcessIO ()
startVnc = do
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
    runProc' name cmd = spawnProcess (ProgramName name) (runExternalProcess name cmd)
    runProc name cmd = superviseProcess (ProgramName name) do
        runExternalProcess name cmd
        error "process died"

demoGUI :: IO ()
demoGUI = do
    v <- runDemo
    putStrLn $ "The end: " <> show v
  where
    htmlMain = do
        doctypehtml_ do
            head_ do
                title_ "My GUI"
                meta_ [charset_ "utf-8"]
                meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
                xstaticScripts xfiles

            with body_ [class_ "font-mono cursor-default bg-stone-100 h-screen"] do
                with div_ [id_ "display-ws", class_ "h-full", makeAttribute "hx-ext" "ws", makeAttribute "ws-connect" "/ws/htmx"] do
                    with div_ [id_ "display-root", class_ "h-full"] mempty

    clientHandler = \case
        UserConnected _ client -> do
            logInfo "Client connected" ["client" .= client]
            spawnPingThread client
            clients <- atomically $ newDisplayClientsFromClient client
            msApp <- mineSweeperApp clients (WinID 0)
            msHtml <- msApp.draw client
            sendHtml client (with div_ [id_ "display-root"] msHtml)

            handleClientEvents client $ \trigger value -> do
                let guiEvent = GuiEvent client trigger value
                logInfo "got ev" ["ev" .= guiEvent]
                atomically $ writePipe msApp.events guiEvent
        UserDisconnected _ client -> logInfo "Client disconnected" ["client" .= client]

    xfiles = [XStatic.htmx, XStatic.htmxExtWS, XStatic.tailwind]
    runDemo =
        withButlerOS do
            logInfo "Starting..." []
            desktop <- superviseProcess "gui" $
                startDisplay 8085 xfiles (const $ pure $ guestAuthApp htmlMain) $ \_ -> do
                    pure $ \_ws -> do
                        env <- ask
                        pure (env, clientHandler)

            void $ awaitProcess desktop

demoDesktop :: IO ()
demoDesktop = do
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
            chat <- atomically newChatServer
            let authApp = invitationAuthApp indexHtml
            desktop <- superviseProcess "desktop" $ startDisplay 8080 xfiles' authApp $ \display -> do
                superviseProcess "chat-service" $ chatServerProgram chat display
                lobbyProgram (appLauncher chat) xinit chat display
            void $ awaitProcess desktop

    xinit desktop = do
        atomically . addApp desktop =<< smApp desktop
        atomically . addApp desktop =<< seatApp desktop

    appLauncher chat desktop name winID = case name of
        "app-chat" -> Just <$> chatApp desktop.hclients winID chat
        "app-clock" -> Just <$> clockApp desktop.hclients winID
        "app-log-viewer" -> Just <$> logViewerApp desktop winID
        "app-term" -> Just <$> termApp desktop winID
        "app-ps" -> Just <$> peApp desktop winID
        "app-vnc" -> Just <$> vncApp desktop winID
        "app-minesweeper" -> Just <$> mineSweeperApp desktop.hclients winID
        "app-tabletop" -> Just <$> tabletopApp desktop winID
        _ -> pure Nothing

    xfiles', xfiles :: [XStaticFile]
    xfiles =
        [ XStatic.sweetAlert2
        , XStatic.hyperscript
        , XStatic.htmx
        , XStatic.htmxExtWS
        , XStatic.tailwind
        , XStatic.remixiconCss
        , XStatic.remixiconWoff2
        , XStatic.logo
        , XStatic.xtermFitAddonJs
        , XStatic.xtermFitAddonJsMap
        , XStatic.winboxCss
        ]
            <> XStatic.xterm
    xfiles' = XStatic.noVNC <> XStatic.winbox <> xfiles

demo :: IO ()
demo = Main.Utf8.withUtf8 do
    -- ensure tty-less environment gets line based output
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering
    -- make rts aware of the cgroup capabilities
    Control.Concurrent.CGroup.initRTSThreads
    getArgs >>= \case
        ["vnc"] -> print =<< withButlerOS startVnc
        _ -> demoDesktop
