module Butler.Core.ProcessorTest where

import Test.Tasty
import Test.Tasty.HUnit

import Butler.Core.Clock
import Butler.Core.Events
import Butler.Core.Logger
import Butler.Core.Processor
import Butler.Prelude

test_processor :: TestTree
test_processor =
    testGroup
        "Processor"
        [ testCase "Create a task" do
            withProcessor' $ \_ _ _ startProcess' -> do
                p <- startProcess' (ProgramName "sleeper") (ProcessAction $ const $ sleep 1)

                exitReason <- atomically $ await p.thread
                exitReason @?= Exited

                status <- atomically $ readTVar p.status
                case status of
                    Stopped (end, reason) -> do
                        reason @?= Exited
                        assertBool ("Invalid process end time: " <> show end) (from @Time @Milli end < 3 && from @Time @Milli end > 0)
                    _ -> assertFailure ("Expected stopped, got " <> show status)
        , testCase "Create tasks" do
            withProcessor' $ \_ logger processor startProcess' -> do
                waiter <- waitLog logger 500 sleeperStarted
                p <- startProcess' (ProgramName "sleeper") sleepAction
                WaitCompleted _ <- atomically waiter
                let lookupProcessIO = atomically . lookupProcess processor
                Just _ <- lookupProcessIO p.pid

                p2 <- startProcess' (ProgramName "sleeper") sleepAction
                -- A process must start in less than 10ms
                sleep 10
                Just _ <- lookupProcessIO p2.pid
                Just _ <- lookupProcessIO p.pid

                -- kill the first task
                True <- atomically $ stopProcess p
                exitReason <- atomically $ await p.thread
                exitReason @?= Killed
                Nothing <- lookupProcessIO p.pid
                Just _ <- lookupProcessIO p2.pid

                -- kill the second task, otherwise the awaitProcessor would last more than 100ms
                True <- atomically $ stopProcess p2
                WaitCompleted _ <- atomically =<< waitTransaction 100 (awaitProcessor processor)
                Nothing <- lookupProcessIO p2.pid
                pure ()
        ]
  where
    withProcessor' cb = withProcessor \processor -> do
        clock <- newClock
        logger <- atomically (newLogger 42)
        cb clock logger processor (startProcess clock logger processor Nothing)
    sleepAction = ProcessAction $ const do
        sleep 1_000
    sleeperStarted = \case
        ProcessCreated p | p.program == ProgramName "sleeper" -> True
        _ -> False
