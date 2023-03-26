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
    module Butler.Core.Pipe,

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
    module Butler.Core.Network,

    -- * Storage API
    newProcessMemory,
    module Butler.Core.Memory,
    StorageAddress (..),
    getPath,
    module Butler.Core.File,

    -- * Display API
    module Butler.Display.Client,
    DisplayEvent (..),

    -- * App API
    App (..),
    AppSharedContext (..),
    AppContext (..),
    defaultApp,
    AppEvent (..),
    UserEvent (..),
    GuiEvent (..),
    DataEvent (..),
    serveApps,
    serveDashboardApps,
    publicDisplayApp,
    Service (..),
    AppID,

    -- * GUI toolkit
    wid_,
    withTrigger,
    withTrigger_,
    withWID,
    dropWID,
    sendHtmlOnConnect,
    module Butler.Display.GUI,
    module Butler.Window,

    -- * Prelude
    module Butler.Prelude,
)
where

import Control.Concurrent.CGroup qualified
import Main.Utf8

import Butler.App
import Butler.AppID
import Butler.Auth
import Butler.Core
import Butler.Core.Clock
import Butler.Core.File
import Butler.Core.Memory
import Butler.Core.Network
import Butler.Core.Pipe
import Butler.Core.Storage
import Butler.Display
import Butler.Display.Client
import Butler.Display.GUI
import Butler.Frame
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
