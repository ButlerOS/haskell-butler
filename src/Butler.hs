{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Butler where

import Data.ByteString qualified as BS
import Data.Text qualified as Text
import System.Environment (getArgs)
import System.Process.Typed

import Butler.Desktop
import Butler.Display
import Butler.Lobby
import Butler.OS
import Butler.Prelude

import Butler.App.Chat
import Butler.App.Clock
import Butler.App.LogViewer
import Butler.App.MineSweeper (mineSweeperApp)
import Butler.App.NoVnc
import Butler.App.ProcessExplorer
import Butler.App.Seat
import Butler.App.SessionManager
import Butler.App.SpeedTest
import Butler.App.Terminal

import Butler.Clock
import Butler.Logger
import Butler.Window

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
    runProc' name cmd = spawnProcess (ProgramName name) (handleProcess name cmd)
    runProc name cmd = superviseProcess (ProgramName name) (handleProcess name cmd)
    handleProcess name cmd = do
        logInfo "Running" ["cmd" .= show cmd]
        withProcessWait_ (setStdout createPipe $ setStderr createPipe cmd) $ \p -> do
            spawnThread_ $ forever do
                buf <- liftIO (BS.hGetLine (getStdout p))
                logTrace name ["stdout" .= BSLog buf]
            forever do
                buf <- liftIO (BS.hGetLine (getStderr p))
                logTrace name ["stderr" .= BSLog buf]

demoDesktop :: IO ()
demoDesktop = do
    v <- runDemo
    putStrLn $ "The end: " <> show v
  where
    runDemo =
        withButlerOS do
            chat <- atomically newChatServer
            desktop <- superviseProcess "desktop" $ startDisplay 8080 $ \display -> do
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
        _ -> pure Nothing

demo :: IO ()
demo =
    getArgs >>= \case
        ["vnc"] -> print =<< withButlerOS startVnc
        _ -> demoDesktop
