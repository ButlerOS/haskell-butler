module Butler.App.SpeedTest where

import Butler.Clock hiding (getTime)
import Butler.Display
import Butler.GUI
import Butler.Prelude

import System.Random

import Data.Map.Strict qualified as Map
import Network.WebSockets qualified as WS

import Butler.Desktop
import Butler.WebSocket (ChannelName)

data SpeedTest = SpeedTest
    { startedAt :: Time
    , transfered :: TVar Int64
    , elapsed :: TVar Time
    }

newtype SpeedTestState = SpeedTestState
    { tests :: TVar (Map Endpoint SpeedTest)
    }

renderApp :: SpeedTestState -> HtmlT STM ()
renderApp state = do
    withWindow "speed-test" 1 do
        with div_ [id_ "speed-test-root"] do
            with button_ [hxTrigger_ "click", wsSend, id_ "start-speed-test", class_ "p-2 rounded bg-green-300"] "Start"
            ul_ do
                tests <- Map.toList <$> lift (readTVar state.tests)
                forM_ tests $ \(endpoint, result) -> do
                    li_ do
                        toHtml endpoint
                        " "
                        Milli elapsed <- from <$> lift (readTVar result.elapsed)
                        transfered <- lift (readTVar result.transfered)
                        toHtml $ show (1000 * (unsafeFrom transfered `div` 1024) `div` elapsed) <> " kb/s"

performTest :: SpeedTest -> DisplayClient -> ProcessIO ()
performTest t client = do
    logInfo_ "running test.."
    start <- getTime
    replicateM_ 50 do
        liftIO $ WS.sendBinaryData client.conn buf
    logInfo_ "waiting"
    replicateM_ 50 do
        recvMessage client
    end <- getTime
    logInfo "completed in" ["elapsed" .= (end - start)]
    atomically do
        writeTVar t.elapsed (end - start)
        writeTVar t.transfered (1024 * 1024)
  where
    pureGen = mkStdGen 137
    buf = fst $ genByteString (1024 * 1024) pureGen

clientScript :: Text
clientScript =
    [str|
function speedTest() {
  console.log("Connecting to", wsUrl)
  var socket = new WebSocket(wsUrl("speedtest"));
  socket.binaryType = 'arraybuffer';
  socket.onopen = (e) => {
    console.log("connected!", e);
  };
  socket.onmessage = (event) => {
    const buf = new Uint8Array(event.data)
    console.log("Got event", event);
    socket.send("Got: " + buf.length)
  };
};
speedTest();
|]

speedTestApp :: Desktop -> ProcessIO (GuiApp, ChannelName, DisplayClient -> ProcessIO ())
speedTestApp desktop = do
    state <- SpeedTestState <$> newTVarIO mempty
    let onClient client = do
            newTest <- SpeedTest <$> getTime <*> newTVarIO 0 <*> newTVarIO 0
            atomically $ modifyTVar' state.tests (Map.insert client.endpoint newTest)
            logInfo "starting speed test" ["endpoint" .= client.endpoint]
            performTest newTest client
            broadcastMessageT desktop (renderApp state)
    (,"speed-test",onClient) <$> newGuiApp "speed-test" Nothing (const $ pure $ renderApp state) ["start-speed-test"] \app -> do
        forever do
            ev <- atomically $ readPipe app.events
            case ev.trigger of
                "start-speed-test" -> do
                    clientMessageT ev.client do
                        with div_ [id_ "speed-test-root", hxSwapOob_ "beforeend"] do
                            "Starting go!"
                            script_ clientScript
                _ -> logError "unknown ev" ["ev" .= ev]
