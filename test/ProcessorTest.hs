module ProcessorTest where

import Test.Hspec.Expectations
import Test.Tasty
import Test.Tasty.HUnit

import Butler.Clock
import Butler.Events
import Butler.Logger
import Butler.Prelude
import Butler.Processor

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
                processor `assertProcessLists` ["<1>sleeper"]

                p2 <- startProcess' (ProgramName "sleeper") sleepAction
                -- A process must start in less than 10ms
                sleep 10
                processor `assertProcessLists` ["<1>sleeper", "<2>sleeper"]

                -- kill the first task
                Just _ <- atomically $ stopProcess processor p.pid
                exitReason <- atomically $ await p.thread
                exitReason @?= Killed
                processor `assertProcessLists` ["<2>sleeper"]
                Nothing <- atomically $ stopProcess processor p.pid

                -- kill the second task, otherwise the awaitProcessor would last more than 100ms
                Just _ <- atomically $ stopProcess processor p2.pid
                WaitCompleted _ <- atomically =<< waitTransaction 100 (awaitProcessor processor)
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

assertProcessLists :: HasCallStack => Processor -> [String] -> Assertion
assertProcessLists processor xs = do
    procs <- fmap show <$> atomically (getProcesses processor)
    procs @?= xs
