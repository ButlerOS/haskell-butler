module Butler.App.NoterTest where

import Butler.App.Noter
import Butler.Prelude
import Butler.Service.FileService (fileService)
import Butler.Test
import Test.Tasty

test_noterApp :: TestTree
test_noterApp = butlerAppTestCase noterApp \appTestContext appInstance -> do
    startTestService appTestContext fileService
    client <- newAppClient appTestContext appInstance

    -- void . superviseProcess "logger" . stdoutLogger . (.logger) =<< asks os

    client `assertReceivedHtml` [Contains "txt", NotContains "save-file"]

    -- Request refresh should be empty
    appInstance `sendEvent` FakeTrigger client "refresh" []
    client `assertReceivedJSON` ["text" .= ("" :: Text)]

    -- First insert should produce a save button
    appInstance `sendEvent` FakeData client ["i" .= ("Helloo" :: Text), "p" .= [0, 0 :: Word]]
    client `assertReceivedHtml` [NotContains "txt", Contains "new-file"]

    -- Delete backward works
    appInstance `sendEvent` FakeData client ["e" .= [0, 6 :: Word], "p" .= [0, 5 :: Word]]
    appInstance `sendEvent` FakeTrigger client "refresh" []
    client `assertReceivedJSON` ["text" .= ("Hello" :: Text)]

    -- Insert more text works
    appInstance `sendEvent` FakeData client ["i" .= (" World" :: Text), "p" .= [0, 5 :: Word]]
    appInstance `sendEvent` FakeTrigger client "refresh" []
    client `assertReceivedJSON` ["text" .= ("Hello World" :: Text)]

runTest :: IO ()
runTest = defaultMain test_noterApp
