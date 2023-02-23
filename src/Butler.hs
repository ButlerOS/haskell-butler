{- | Welcome to Butler.

This module exposes the main API.
-}
module Butler (
    -- * Core primitive
    runMain,
    ProcessIO,
    spawnInitProcess,

    -- * Concurrency API
    Process (..),
    ProgramName (..),
    spawnProcess,
    waitProcess,
    stopProcess,
    killProcess,
    spawnThread_,
    spawnThread,
    module Butler.Pipe,

    -- * Logging API
    logInfo,
    logError,
    logDebug,

    -- * Time API
    Time,
    getTime,
    Milli,
    sleep,
    WaitResult (..),
    waitTransaction,

    -- * Network API
    module Butler.Network,

    -- * Display API
    module Butler.DisplayClient,
    DisplayEvent (..),

    -- * App API
    App (..),
    AppContext (..),
    defaultApp,
    AppEvent (..),
    GuiEvent (..),
    DataEvent (..),
    serveApps,
    serveDashboardApps,
    publicDisplayApp,

    -- * GUI toolkit
    sendHtmlOnConnect,
    module Butler.GUI,
    module Butler.Window,

    -- * Prelude
    module Butler.Prelude,
)
where

import Control.Concurrent.CGroup qualified
import Main.Utf8

import Butler.App
import Butler.Auth
import Butler.Clock
import Butler.Display
import Butler.DisplayClient
import Butler.Frame
import Butler.GUI
import Butler.Network
import Butler.OS
import Butler.Pipe
import Butler.Prelude
import Butler.Window

-- | Helper to setup line-buffering and cgroup rts for container
runMain :: IO a -> IO a
runMain action = Main.Utf8.withUtf8 do
    -- ensure tty-less environment gets line based output
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering
    -- make rts aware of the cgroup capabilities
    Control.Concurrent.CGroup.initRTSThreads
    action
