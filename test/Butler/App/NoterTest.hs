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

    client `assertReceivedHtml` [Contains "txt-div", NotContains "save-file"]

    -- Request refresh should be empty
    appInstance `sendEvent` FakeTrigger client "refresh" []
    client `assertReceivedJSON` ["text" .= ("" :: Text)]

    -- First insert should produce a save button
    appInstance `sendEvent` FakeData client ["insert" .= ("Helloo" :: Text)]
    client `assertReceivedHtml` [NotContains "txt-div", Contains "new-file"]

    -- Delete backward works
    appInstance `sendEvent` FakeData client ["delete" .= (-1 :: Int)]
    appInstance `sendEvent` FakeTrigger client "refresh" []
    client `assertReceivedJSON` ["text" .= ("Hello" :: Text)]

    -- Sending more text should append
    appInstance `sendEvent` FakeData client ["insert" .= (" World" :: Text)]
    appInstance `sendEvent` FakeTrigger client "refresh" []
    client `assertReceivedJSON` ["text" .= ("Hello World" :: Text)]

    -- Move cursor and insert
    appInstance `sendEvent` FakeData client ["move" .= (2 :: Int)]
    appInstance `sendEvent` FakeData client ["insert" .= ("e" :: Text)]
    appInstance `sendEvent` FakeTrigger client "refresh" []
    client `assertReceivedJSON` ["text" .= ("Heello World" :: Text)]

    -- Delete forward works
    appInstance `sendEvent` FakeData client ["delete" .= (1 :: Int)]
    appInstance `sendEvent` FakeTrigger client "refresh" []
    client `assertReceivedJSON` ["text" .= ("Heelo World" :: Text)]

    -- Move and delete works
    appInstance `sendEvent` FakeData client ["move" .= (42 :: Int)]
    appInstance `sendEvent` FakeData client ["delete" .= (-42 :: Int)]
    appInstance `sendEvent` FakeTrigger client "refresh" []
    client `assertReceivedJSON` ["text" .= ("" :: Text)]

runTest :: IO ()
runTest = defaultMain test_noterApp
