module Butler.OS (
    -- * Boot
    OS (..),
    withButlerOS,
    ProcessEnv (..),
    ProcessIO,
    runProcessIO,
    getSelfProcess,
    asProcess,
    runExternalProcess,

    -- * Memory api
    newProcessMemory,

    -- * Processor api
    spawnProcess,
    superviseProcess,
    killProcess,
    spawnThread_,

    -- * Log api
    logSystem,
    logTrace,
    logInfo_,
    logInfo,
    logError_,
    logError,

    -- * Clock api
    getTime,

    -- * Helpers
    awaitProcess,

    -- * Re-exports
    ProgramName (..),
    Process (..),
    ProcessAction (..),
    SystemEvent (..),
) where

import Control.Retry
import Data.ByteString qualified as BS
import System.Process.Typed hiding (Process, startProcess, stopProcess)

import Butler.Buzzer
import Butler.Clock hiding (getTime)
import Butler.Clock qualified as Clock
import Butler.Events
import Butler.Logger
import Butler.Memory
import Butler.Prelude
import Butler.Process
import Butler.Processor
import Butler.Storage

import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.OneLine (renderObject)
import Data.Aeson.Types (Pair)
import Ki.Unlifted qualified as Ki
import System.IO.Error (isEOFError)

newtype ProcessIO a = ProcessIO (ProcessEnv -> IO a)
    deriving
        ( Functor
        , Applicative
        , Monad
        , MonadFail
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
runProcessIO os process (ProcessIO action) = action (ProcessEnv os process)

getSelfProcess :: ProcessIO Process
getSelfProcess = asks process

logSystem :: EventSeverity -> SystemEvent -> ProcessIO ()
logSystem s ev = do
    os <- asks os
    now <- liftIO os.clock.getTime
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
    liftIO os.clock.getTime

getLocName :: HasCallStack => ByteString
getLocName = case getCallStack callStack of
    (_logStack : (_, srcLoc) : _) -> encodeUtf8 $ from (srcLocModule srcLoc) <> ":" <> from (show (srcLocStartLine srcLoc))
    _ -> "N/C"

logTrace :: HasCallStack => Text -> [Pair] -> ProcessIO ()
logTrace = processLog getLocName EventTrace

logInfo_ :: HasCallStack => Text -> ProcessIO ()
logInfo_ msg = processLog getLocName EventInfo msg []

logInfo :: HasCallStack => Text -> [Pair] -> ProcessIO ()
logInfo = processLog getLocName EventInfo

logError :: HasCallStack => Text -> [Pair] -> ProcessIO ()
logError = processLog getLocName EventError

logError_ :: HasCallStack => Text -> ProcessIO ()
logError_ msg = processLog getLocName EventInfo msg []

newProcessMemory :: Serialise a => StorageAddress -> ProcessIO a -> ProcessIO (a, MemoryVar a)
newProcessMemory addr initialize = do
    os <- asks os
    p <- asks process
    liftIO $ newMemoryVar os.storage addr (runProcessIO os p initialize)

runExternalProcess :: Text -> ProcessConfig stdin stdout0 stderr0 -> ProcessIO ()
runExternalProcess name cmd = do
    logInfo "Running" ["cmd" .= show cmd]
    withProcessWait_ (setStdout createPipe $ setStderr createPipe cmd) $ \p -> do
        stdoutFlusher <- spawnThread $ handle eofHandler $ forever do
            buf <- liftIO (BS.hGetLine (getStdout p))
            logTrace name ["stdout" .= BSLog buf]
        handle eofHandler $ forever do
            buf <- liftIO (BS.hGetLine (getStderr p))
            logTrace name ["stderr" .= BSLog buf]
        atomically (await stdoutFlusher)
  where
    eofHandler e
      | isEOFError e = pure ()
      | otherwise = throwIO e

spawnProcess :: ProgramName -> ProcessIO () -> ProcessIO Process
spawnProcess name (ProcessIO action) = do
    env <- ask
    let scopedAction = ProcessAction (\p -> action (ProcessEnv env.os p))
    liftIO $
        startProcess env.os.clock env.os.logger env.os.processor (Just env.process) name scopedAction

asProcess :: ProcessEnv -> ProcessIO a -> ProcessIO a
asProcess env = local (const env)

spawnThread_ :: ProcessIO Void -> ProcessIO ()
spawnThread_ action = do
    process <- asks process
    process.scope `Ki.fork_` action

spawnThread :: ProcessIO a -> ProcessIO (Thread a)
spawnThread action = do
    process <- asks process
    process.scope `Ki.fork` action

killProcess :: Pid -> ProcessIO (Maybe Process)
killProcess pid = do
    os <- asks os
    atomically $ stopProcess os.processor pid

data OS = OS
    { processor :: Processor
    , storage :: Storage
    , clock :: Clock
    , logger :: Logger SystemEvent
    , buzzer :: Buzzer
    }
    deriving (Generic)

awaitProcess :: MonadIO m => Process -> m ExitReason
awaitProcess p = atomically $ await p.thread

withButlerOS :: ProcessIO () -> IO ExitReason
withButlerOS action = withProcessor \processor -> do
    clock <- newClock
    logger <- atomically (newLogger 42)
    storage <- newStorage ".butler-storage"

    let os = OS processor storage clock logger newBuzzer
    os.buzzer 440

    let systemDaemons = do
            void $ superviseProcess "logger" (stdoutLogger os.logger)
            void $ superviseProcess "storage" (syncThread os.storage (logSystem EventInfo . StorageSync))

    let createProcess mb = startProcess mb.clock mb.logger mb.processor
    p <- createProcess os Nothing "init" $ ProcessAction $ \process ->
        runProcessIO os process do
            systemDaemons
            -- wait for daemon to initialize
            sleep 1
            logSystem EventInfo SystemReady
            action
            logSystem EventInfo SystemCompleted

    atomically $ await p.thread

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

-- | The list of retry and their delay time in ms
_restartPolicySimulation :: ProcessIO [(Int, Int)]
_restartPolicySimulation = fmap (fmap (flip div 1_000 . fromMaybe 0)) <$> simulatePolicy 10 supervisorRestartPolicy

supervisorRestartPolicy :: RetryPolicyM ProcessIO
supervisorRestartPolicy = fullJitterBackoff 150_000 <> limitRetries 3
