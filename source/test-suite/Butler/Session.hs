module Butler.Session where

import Butler
import Butler.Display.Session
import Butler.Test
import Test.Tasty
import Test.Tasty.HUnit

test_sessions :: TestTree
test_sessions = butlerTestCase "Butler.Session" do
    withSessions ":memory:" \sessions -> do
        session <- newSession sessions Nothing "guest"

        -- session can change to new username
        session `hasUsername` "guest"
        expectTrue <- changeUsername sessions session "alice"
        session `hasUsername` "alice"
        liftIO $ expectTrue @?= True

        -- session can't change to existing username
        otherSession <- newSession sessions Nothing "guest"
        otherSession `hasUsername` "guest"
        expectFalse <- changeUsername sessions otherSession "alice"
        otherSession `hasUsername` "guest"
        liftIO $ expectFalse @?= False

        -- new session gets unique username
        dupSession <- newSession sessions Nothing "guest"
        dupSession `hasUsername` "guest_1"

        -- session can change to new provider
        let aliceProvider = externalProvider "pam" "alice"
        changeProvider sessions session aliceProvider

        -- session can be searched by provider
        mAliceSession <- atomically do lookupSessionByProvider sessions aliceProvider
        liftIO do
            (.sessionID) <$> mAliceSession @?= Just session.sessionID

        -- and by username
        mAliceSession2 <- atomically do lookupSessionByUser sessions "alice"
        liftIO do
            (.sessionID) <$> mAliceSession2 @?= Just session.sessionID
  where
    hasUsername session username = do
        sessionUsername <- readTVarIO session.username
        liftIO $ sessionUsername @?= username
