module Butler.App.NoterTest where

-- import Butler.Core
-- import Butler.Core.Logger
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

    client `assertReceivedHtml` [Contains "editor", NotContains "save-file"]
    client `assertReceivedJSON` ["body" .= ("" :: Text), "rev" .= (0 :: Word)]

    -- Request refresh should be empty
    appInstance `sendEvent` FakeTrigger client "refresh" []
    client `assertReceivedJSON` ["body" .= ("" :: Text), "rev" .= (0 :: Word)]

    -- First insert should produce a save button
    appInstance `sendEvent` FakeValue client (toJSON (0 :: Word, ["Helloo" :: Text]))
    client `assertReceivedJSON` ["rev" .= (1 :: Int)]
    client `assertReceivedHtml` [NotContains "editor", Contains "new-file"]

    -- Delete backward works
    appInstance `sendEvent` FakeValue client (toJSON (1 :: Word, [5 :: Int, -1]))
    client `assertReceivedJSON` ["rev" .= (2 :: Int)]
    appInstance `sendEvent` FakeTrigger client "refresh" []
    client `assertReceivedJSON` ["body" .= ("Hello" :: Text), "rev" .= (2 :: Int)]

    -- Insert more text works
    appInstance `sendEvent` FakeValue client (toJSON (2 :: Word, [toJSON (5 :: Int), toJSON (" World" :: Text)]))
    client `assertReceivedJSON` ["rev" .= (3 :: Int)]
    appInstance `sendEvent` FakeTrigger client "refresh" []
    client `assertReceivedJSON` ["body" .= ("Hello World" :: Text), "rev" .= (3 :: Int)]

runTest :: IO ()
runTest = defaultMain test_noterApp
