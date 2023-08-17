module Butler.Session where

import Butler.Display.Session
import Butler.Prelude
import Butler.Test
import Test.Tasty
import Test.Tasty.HUnit

test_sessions :: TestTree
test_sessions = butlerTestCase "Butler.Session" do
    withSessions ":memory:" \sessions -> do
        session <- newSession sessions localProvider "guest"

        expectTrue <- changeUsername sessions session (externalProvider "GH") "TristanCacqueray"
        liftIO $ expectTrue @?= True

        otherSession <- newSession sessions localProvider "guest"
        expectFalse <- changeUsername sessions otherSession (externalProvider "GH") "TristanCacqueray"
        liftIO $ expectFalse @?= False
