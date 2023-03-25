module Butler.Test where

import Butler.App
import Butler.Core
import Butler.Core.Clock
import Butler.Core.Process
import Butler.Display
import Butler.Display.GUI
import Butler.Display.Session
import Butler.Frame
import Butler.Prelude

import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Data.UUID.V4 qualified as UUID
import Network.WebSockets qualified as WS
import Network.WebSockets.Client qualified as WS
import Network.WebSockets.Connection qualified as WS

import Test.Tasty (TestName, TestTree)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

data AppTestContext = AppTestContext
    { clients :: DisplayClients
    , shared :: AppSharedContext
    }

butlerTestCase :: TestName -> ProcessIO () -> TestTree
butlerTestCase name action = testCase name do
    res <- spawnRawInitProcess (const $ pure ()) ".butler-test" action
    res @?= Exited

newTestClient :: AppSharedContext -> ProcessIO DisplayClient
newTestClient appSharedContext = do
    sessionID <- SessionID <$> liftIO UUID.nextRandom
    session <- atomically (Session sessionID <$> newTVar "tester" <*> newTVar False)
    process <- spawnProcess "test-client" (forever $ sleep 60_000)
    closeRef <- newIORef False
    let
        testConn = WS.Connection WS.defaultConnectionOptions undefined WS.Hybi13 (pure Nothing) (const (pure ())) closeRef
    client <- atomically (newClient testConn "localhost:4242" process session)
    atomically (modifyTVar' appSharedContext.display.clients $ Map.insert sessionID [client])
    pure client

newAppClient :: AppTestContext -> AppInstance -> ProcessIO DisplayClient
newAppClient appTestContext appInstance = do
    client <- newTestClient appTestContext.shared
    atomically $ addClient appTestContext.clients client
    writePipe appInstance.pipe (AppDisplay $ UserJoined client)
    pure client

withTestSharedContext :: AppSet -> (AppSharedContext -> ProcessIO ()) -> ProcessIO ()
withTestSharedContext appSet cb = withSessions ":memory:" \sessions -> do
    processEnv <- ask
    appSharedContext <- atomically do
        display <- Display sessions <$> newTVar mempty
        newAppSharedContext display processEnv appSet
    cb appSharedContext

withAppInstance :: App -> (AppTestContext -> AppInstance -> ProcessIO ()) -> ProcessIO ()
withAppInstance app cb = withTestSharedContext (newAppSet [app]) \appSharedContext -> do
    clients <- atomically newDisplayClients
    appInstance <- startApp "app" app appSharedContext clients (WinID 1)
    cb (AppTestContext clients appSharedContext) appInstance

startTestService :: AppTestContext -> Service -> ProcessIO ()
startTestService appTestContext (Service app) = do
    -- TODO: increase win-id for next service
    void $ startApp "srv" app appTestContext.shared appTestContext.clients (WinID 2)

butlerAppTestCase :: App -> (AppTestContext -> AppInstance -> ProcessIO ()) -> TestTree
butlerAppTestCase app cb = butlerTestCase ("Butler.App." <> via @Text app.name) do
    withAppInstance app cb

assertContains :: HasCallStack => Text -> Text -> ProcessIO ()
assertContains got expected
    | expected `Text.isInfixOf` got = pure ()
    | otherwise = liftIO $ assertFailure (into @String $ "Expected: " <> expected <> ", got: " <> got)

assertNotContains :: HasCallStack => Text -> Text -> ProcessIO ()
assertNotContains got expected
    | expected `Text.isInfixOf` got = liftIO $ assertFailure (into @String $ "Didn't expect: " <> expected <> ", got: " <> got)
    | otherwise = pure ()

data ContentTest = Contains Text | NotContains Text

assertContentTest :: Text -> ContentTest -> ProcessIO ()
assertContentTest got = \case
    Contains expected -> assertContains got expected
    NotContains avoid -> assertNotContains got avoid

assertReceivedHtml :: HasCallStack => DisplayClient -> [ContentTest] -> ProcessIO ()
assertReceivedHtml client contentTests = do
    msg <- atomically =<< waitTransaction 100 (readTChan client.sendChannel)
    case msg of
        WaitTimeout -> fail "Receive timeout"
        WaitCompleted (WS.Binary buf) -> liftIO $ assertFailure $ "Expected html, received binary: " <> show buf
        WaitCompleted (WS.Text buf mTxt) ->
            let got = maybe (decodeUtf8 (from buf)) from mTxt
             in traverse_ (assertContentTest got) contentTests

assertReceivedJSON :: HasCallStack => DisplayClient -> [Pair] -> ProcessIO ()
assertReceivedJSON client attrs = do
    msg <- atomically =<< waitTransaction 100 (readTChan client.sendChannel)
    case msg of
        WaitTimeout -> fail "Receive timeout"
        WaitCompleted (WS.Text buf _) -> liftIO $ assertFailure $ "Expected binary, received: " <> show buf
        WaitCompleted (WS.Binary buf) -> case fmap (decodeJSON . from) <$> decodeMessage (from buf) of
            Just (_, Just (v :: Value)) ->
                when (v /= object attrs) do
                    liftIO $ assertFailure $ into @String $ decodeUtf8 $ from $ "Expected: " <> encodeJSON attrs <> ", got " <> encodeJSON v
            _ -> liftIO $ assertFailure $ "Invalid binary payload: " <> show buf

data FakeEvent
    = FakeTrigger DisplayClient TriggerName [Pair]
    | FakeData DisplayClient [Pair]

sendEvent :: AppInstance -> FakeEvent -> ProcessIO ()
sendEvent appInstance = \case
    FakeTrigger client trigger attrs ->
        writePipe appInstance.pipe $ AppTrigger (GuiEvent client trigger (object attrs))
    FakeData client attrs ->
        let buf = encodeJSON (object attrs)
            rawBuf = encodeMessage 0 buf
         in writePipe appInstance.pipe $ AppData (DataEvent client (from buf) (from rawBuf))
