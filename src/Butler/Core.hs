module Butler.Core (
    -- * Boot
    OS (..),
    spawnInitProcess,
    spawnRawInitProcess,
    withButlerOS,
    ProcessEnv (..),
    ProcessIO,
    runProcessIO,
    runProcessIOEnv,
    getSelfProcess,
    asProcess,
    runExternalProcess,

    -- * Memory api
    getPath,
    newProcessMemory,
    chroot,

    -- * Processor api
    spawnProcess,
    superviseProcess,
    stopProcess,
    killProcess,
    spawnThread_,
    spawnThread,

    -- * Log api
    logSystem,
    logDebug,
    logInfo,
    logError,

    -- * Clock api
    getTime,

    -- * IPC api
    writePipe,

    -- * Helpers
    waitProcess,

    -- * Re-exports
    ProgramName (..),
    Process (..),
    ProcessAction (..),
    SystemEvent (..),
) where

import Control.Retry
import Data.ByteString qualified as BS
import System.Process.Typed hiding (Process, startProcess, stopProcess)

import Butler.Core.Buzzer
import Butler.Core.Clock
import Butler.Core.Events
import Butler.Core.Logger
import Butler.Core.Memory
import Butler.Core.Pipe
import Butler.Core.Process
import Butler.Core.Processor
import Butler.Core.Storage
import Butler.Prelude

import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.OneLine (renderObject)
import Ki.Unlifted qualified as Ki
import System.IO.Error (isEOFError)

newtype ProcessIO a = ProcessIO (ProcessEnv -> IO a)
    deriving
        ( Functor
        , Applicative
        , Monad
        , MonadFail
        , MonadThrow
        , MonadCatch
        , MonadMask
        , MonadIO
        , MonadReader ProcessEnv
        , MonadUnliftIO
        )
        via ReaderT ProcessEnv IO

data ProcessEnv = ProcessEnv
    { os :: OS
    , process :: Process
    }
    deriving (Generic)

runProcessIO :: OS -> Process -> ProcessIO a -> IO a
runProcessIO os process = runProcessIOEnv (ProcessEnv os process)

runProcessIOEnv :: ProcessEnv -> ProcessIO a -> IO a
runProcessIOEnv env (ProcessIO action) = action env

getSelfProcess :: ProcessIO Process
getSelfProcess = asks process

logSystem :: EventSeverity -> SystemEvent -> ProcessIO ()
logSystem s ev = do
    os <- asks os
    now <- getTime
    atomically (addEvent os.logger now s ev)

processLog :: ByteString -> EventSeverity -> Text -> [Pair] -> ProcessIO ()
processLog loc s msg attrs = do
    p <- asks process
    logSystem s (ProcessMessage loc p msgText)
  where
    msgText = case attrs of
        [] -> msg
        _ -> msg <> " " <> Data.Aeson.OneLine.renderObject (KM.fromList attrs)

getTime :: ProcessIO Time
getTime = do
    os <- asks os
    getClockTime os.clock

getPath :: StorageAddress -> ProcessIO RawFilePath
getPath addr = do
    os <- asks os
    pure $ getStoragePath os.storage addr

getLocName :: HasCallStack => ByteString
getLocName = case getCallStack callStack of
    (_logStack : (_, srcLoc) : _) -> encodeUtf8 $ from (srcLocModule srcLoc) <> ":" <> from (show (srcLocStartLine srcLoc))
    _ -> "N/C"

-- | Use 'logInfo' for nominal but important event.
logInfo :: HasCallStack => Text -> [Pair] -> ProcessIO ()
logInfo = processLog getLocName EventInfo

-- | Use 'logError' for unexpected mal-functions.
logError :: HasCallStack => Text -> [Pair] -> ProcessIO ()
logError = processLog getLocName EventError

-- | Use 'logDebug' for the rest.
logDebug :: HasCallStack => Text -> [Pair] -> ProcessIO ()
logDebug = processLog getLocName EventDebug

newProcessMemory :: Serialise a => StorageAddress -> ProcessIO a -> ProcessIO (a, MemoryVar a)
newProcessMemory addr initialize = do
    os <- asks os
    newMemoryVar logError os.storage addr initialize

-- | Change the root storage directory of a process.
chroot :: StorageAddress -> ProcessIO a -> ProcessIO a
chroot addr action = do
    os <- asks os
    localStorage <- scopeStorage os.storage addr
    local (#os . #storage .~ localStorage) action

runExternalProcess :: Text -> ProcessConfig stdin stdout0 stderr0 -> ProcessIO ()
runExternalProcess name cmd = do
    logInfo "Running" ["cmd" .= show cmd]
    withProcessWait_ (setStdout createPipe $ setStderr createPipe cmd) $ \p -> do
        stdoutFlusher <- spawnThread $ handle eofHandler $ forever do
            buf <- liftIO (BS.hGetLine (getStdout p))
            logDebug name ["stdout" .= BSLog buf]
        handle eofHandler $ forever do
            buf <- liftIO (BS.hGetLine (getStderr p))
            logDebug name ["stderr" .= BSLog buf]
        atomically (await stdoutFlusher)
  where
    eofHandler e
        | isEOFError e = pure ()
        | otherwise = throwIO e

-- | Create a process.
spawnProcess :: ProgramName -> ProcessIO () -> ProcessIO Process
spawnProcess name (ProcessIO action) = do
    env <- ask
    let scopedAction = ProcessAction (\p -> action (ProcessEnv env.os p))
    liftIO $
        startProcess env.os.clock env.os.logger env.os.processor (Just env.process) name scopedAction

asProcess :: ProcessEnv -> ProcessIO a -> ProcessIO a
asProcess env = local (const env)

{- | Create a child thread that never exit, for example to handle messages.
If the thread crash, the whole process is terminated.
-}
spawnThread_ :: ProcessIO Void -> ProcessIO ()
spawnThread_ action = do
    process <- asks process
    process.scope `Ki.fork_` action

-- | Create a child thread.
spawnThread :: ProcessIO a -> ProcessIO (Thread a)
spawnThread action = do
    process <- asks process
    process.scope `Ki.fork` action

-- | Lookup and kill the pid. Use 'stopProcess' instead.
killProcess :: Pid -> ProcessIO Bool
killProcess pid = do
    os <- asks os
    atomically (lookupProcess os.processor pid) >>= \case
        Nothing -> do
            logError "Unknown pid" ["pid" .= pid]
            pure False
        Just p -> atomically (stopProcess p)

data OS = OS
    { processor :: Processor
    , storage :: Storage
    , clock :: Clock
    , logger :: Logger SystemEvent
    , buzzer :: Buzzer
    }
    deriving (Generic)

waitProcess :: MonadIO m => Process -> m ExitReason
waitProcess p = atomically $ await p.thread

writePipe :: HasCallStack => Pipe a -> a -> ProcessIO ()
writePipe p v = unlessM (atomically (tryWritePipe p v)) do
    logError "Write pipe failed!" []

spawnRawInitProcess :: (OS -> ProcessIO ()) -> RawFilePath -> ProcessIO a -> IO ExitReason
spawnRawInitProcess systemAction fp action = withProcessor \processor -> do
    clock <- newClock
    logger <- atomically (newLogger 42)
    storage <- newStorage fp

    let os = OS processor storage clock logger newBuzzer
    os.buzzer 440

    let createProcess mb = startProcess mb.clock mb.logger mb.processor
    p <- createProcess os Nothing "init" $ ProcessAction $ \process ->
        runProcessIO os process do
            systemAction os
            -- wait for daemon to initialize
            sleep 1
            logSystem EventInfo SystemReady
            _ <- action
            logSystem EventInfo SystemCompleted

    atomically $ await p.thread

-- | Run the initial process with a given storage root directory.
spawnInitProcess :: RawFilePath -> ProcessIO a -> IO ExitReason
spawnInitProcess = spawnRawInitProcess startDaemons
  where
    startDaemons os = do
        void $ superviseProcess "logger" (stdoutLogger os.logger)
        void $ superviseProcess "storage" (syncThread os.storage (logSystem EventInfo . StorageSync))

withButlerOS :: ProcessIO a -> IO ExitReason
withButlerOS = spawnInitProcess ".butler-storage"

superviseProcess :: ProgramName -> ProcessIO Void -> ProcessIO Process
superviseProcess name action = do
    parent <- asks process
    let supervisor :: RetryStatus -> ProcessIO ()
        supervisor retryStatus = do
            -- Spawn the process to be supervised
            process <- spawnProcess name do
                _ <- action
                die "The impossible happend, void got created"

            -- Wait for the process
            res <- atomically $ await process.thread
            logSystem EventError (DaemonCrashed process retryStatus.rsIterNumber)

            -- Check
            case res of
                Killed{} -> die "Daemon got killed"
                Exited{} -> die "Daemon exited"
                Crashed{} -> do
                    newStatusM <- applyAndDelay supervisorRestartPolicy retryStatus
                    case newStatusM of
                        Just newStatus -> supervisor newStatus
                        Nothing -> void $ killProcess parent.pid

    spawnProcess ("supervisor-" <> name) do
        supervisor defaultRetryStatus

{-
-- | The list of retry and their delay time in ms
_restartPolicySimulation :: ProcessIO [(Int, Int)]
_restartPolicySimulation = fmap (fmap (flip div 1_000 . fromMaybe 0)) <$> simulatePolicy 10 supervisorRestartPolicy
-}

supervisorRestartPolicy :: RetryPolicyM ProcessIO
supervisorRestartPolicy = fullJitterBackoff 150_000 <> limitRetries 3
