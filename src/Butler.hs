{- | Welcome to Butler.

This module exposes the main API.
-}
module Butler (
    -- * Core primitive
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
    AppStart,
    AppEvent (..),
    GuiEvent (..),
    DataEvent (..),
    serveAppPerClient,
    singleGuestDisplayApp,

    -- * GUI toolkit
    sendHtmlOnConnect,
    module Butler.GUI,
    module Butler.Window,

    -- * Prelude
    module Butler.Prelude,
)
where

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
