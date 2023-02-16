module Butler.Processor (
    Processor,
    withProcessor,
    awaitProcessor,
    Process (..),
    ProcessAction (..),
    lookupChildProcess,
    lookupProcess,
    startProcess,
    stopProcess,

    -- * useful re-exports
    module Butler.Process,
) where

import Butler.Clock
import Butler.Events
import Butler.Logger
import Butler.NatMap as NM
import Butler.Prelude
import Butler.Process

data Processor = Processor
    { scope :: Scope
    , nextPID :: NatCounter
    , rootProcesses :: TVar [Process]
    }

awaitProcessor :: Processor -> STM ()
awaitProcessor processor = awaitAll processor.scope

withProcessor :: (Processor -> IO a) -> IO a
withProcessor cb = scoped \scope -> do
    processor <- atomically (newProcessor scope)
    cb processor

newProcessor :: Scope -> STM Processor
newProcessor scope = Processor scope <$> newNatCounter <*> newTVar []

newtype ProcessAction = ProcessAction (Process -> IO ())

lookupProcess :: Processor -> Pid -> STM (Maybe Process)
lookupProcess processor pid =
    traverseChilds =<< readTVar processor.rootProcesses
  where
    traverseChilds [] = pure Nothing
    traverseChilds (x : xs) =
        lookupChildProcess x pid >>= \case
            Just p -> pure (Just p)
            Nothing -> traverseChilds xs

lookupChildProcess :: Process -> Pid -> STM (Maybe Process)
lookupChildProcess process pid
    | process.pid == pid = pure (Just process)
    | otherwise = traverseChilds =<< readTVar process.childs
  where
    traverseChilds [] = pure Nothing
    traverseChilds (x : xs) =
        lookupChildProcess x pid >>= \case
            Just p -> pure (Just p)
            Nothing -> traverseChilds xs

stopProcess :: Process -> STM Bool
stopProcess process = tryPutTMVar process.doneVar ()

startProcess ::
    Clock ->
    Logger SystemEvent ->
    Processor ->
    Maybe Process ->
    ProgramName ->
    ProcessAction ->
    IO Process
startProcess clock logger processor parent program (ProcessAction action) = do
    createdAt <- getTime clock
    doneVar <- newEmptyTMVarIO
    mthread <- newEmptyTMVarIO
    mprocess <- newEmptyTMVarIO

    let parentScope = case parent of
            Just parentProcess -> parentProcess.scope
            Nothing -> processor.scope

    let parentChilds = case parent of
            Just parentProcess -> parentProcess.childs
            Nothing -> processor.rootProcesses

    let createProcess :: Scope -> ThreadId -> STM Process
        createProcess scope threadId = do
            pid <- Pid <$> incr processor.nextPID
            status <- newTVar Running
            thread <- readTMVar mthread
            childs <- newTVar []

            let process = Process{..}

            modifyTVar' parentChilds (process :)

            putTMVar mprocess process
            pure process

    let processTerminated :: Time -> Maybe (Either SomeException ()) -> STM ExitReason
        processTerminated now res = do
            let exitReason = case res of
                    Nothing -> Killed
                    Just (Right ()) -> Exited
                    Just (Left e) -> Crashed e

            process <- readTMVar mprocess
            writeTVar process.status $ Stopped (now, exitReason)
            addEvent logger now EventInfo (ProcessStopped process exitReason)
            modifyTVar' parentChilds (filter (\p -> p.pid /= process.pid))

            pure exitReason

    -- Create a new scope
    thread <-
        parentScope `fork` scoped \scope -> do
            now <- getTime clock
            processThread <-
                scope `forkTry` do
                    scoped \processScope -> do
                        threadId <- myThreadId
                        process <- atomically do
                            process <- createProcess processScope threadId
                            addEvent logger now EventInfo (ProcessCreated process)
                            pure process
                        action process
            res <- atomically (hitman processThread doneVar)
            end <- getTime clock
            atomically (processTerminated end res)

    -- Write back the new ki thread so that it is part of the Process data
    atomically $ putTMVar mthread thread

    -- Wait for child thread to be running
    atomically (readTMVar mprocess)

hitman :: Thread a -> TMVar () -> STM (Maybe a)
hitman processThread doneVar = waitDoneVar <|> waitProcess
  where
    waitDoneVar = Nothing <$ readTMVar doneVar
    waitProcess = Just <$> await processThread
