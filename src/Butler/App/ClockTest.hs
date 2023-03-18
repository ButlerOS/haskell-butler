module Butler.App.ClockTest where

import Butler.App.Clock
import Butler.Prelude
import Butler.Test
import Test.Tasty

test_clockApp :: TestTree
test_clockApp = butlerAppTestCase clockApp \appTestContext appInstance -> do
    client <- newAppClient appTestContext appInstance
    client `assertReceivedHtml` [Contains "clock-tz"]
    -- Check default clock is UTC
    client `assertReceivedHtml` [Contains "clock-value", Contains "UTC", NotContains "GMT"]
    -- Click on GMT
    appInstance `sendEvent` FakeTrigger client "clock-tz" ["v" .= ("GMT" :: Text)]
    -- Check new clock is GMT
    client `assertReceivedHtml` [Contains "clock-value", Contains "GMT", NotContains "UTC"]
    -- Invalid value are ignoed
    appInstance `sendEvent` FakeTrigger client "clock-tz" ["v" .= ("BAD" :: Text)]
    client `assertReceivedHtml` [Contains "clock-value", Contains "GMT", NotContains "UTC"]

runTest :: IO ()
runTest = defaultMain test_clockApp
