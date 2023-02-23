module Butler.App.NoVnc (vncApp) where

import Butler
import XStatic.NoVNC qualified as XStatic

import Ki.Unlifted qualified as Ki
import Network.Run.TCP (runTCPClient)
import Network.Socket.ByteString (recv, sendAll)

renderTray :: WinID -> HtmlT STM ()
renderTray wid = do
    with span_ [id_ (withWID wid "tray"), class_ "inline-block bg-stone-600 mx-1 px-1"] do
        span_ do
            "NoVNC"
        with span_ [class_ "border border-gray-500 ml-2 inline-flex rounded-md"] do
            with
                i_
                [ class_ "mx-1 cursor-pointer ri-fullscreen-line"
                , onclick_ $ showScript "vnc"
                ]
                mempty
            with
                i_
                [ class_ "mx-1 cursor-pointer ri-subtract-line"
                , onclick_ $ hideScript "vnc"
                ]
                mempty

data VncServer = VncServer
    { host :: String
    , port :: String
    , dim :: TVar (Int, Int)
    }

newVncServer :: Text -> Int -> STM VncServer
newVncServer h p = VncServer (from h) (show p) <$> newTVar (800, 600)

vncApp :: App
vncApp =
    (defaultApp "vnc" startVncApp)
        { tags = fromList ["Graphic", "Development"]
        , description = "NoVNC client"
        , xfiles = XStatic.noVNC
        }

startVncApp :: AppStart
startVncApp _clients wid pipeAE = do
    srv <- atomically (newVncServer "localhost" 5900)

    let draw :: HtmlT STM ()
        draw = do
            with div_ [wid_ wid "w", class_ "truncate relative overflow-hidden"] do
                topRightMenu
                    [ with
                        div_
                        [class_ "cursor-pointer bg-grey-200 bg-opacity-90", wsSend, id_ (withWID wid "change-quality")]
                        "high-res"
                    , with
                        div_
                        [class_ "cursor-pointer bg-grey-200 bg-opacity-90", wsSend, id_ (withWID wid "change-res")]
                        "1280x1024"
                    ]
                with div_ [wid_ wid "vnc", class_ "overflow-hidden"] mempty
                dim <- lift (readTVar srv.dim)
                with (script_ $ vncClient wid dim) [type_ "module"]
            renderTray wid

    let onClient client = do
            logInfo "new client" ["client" .= client]
            withRunInIO \runInIO -> runTCPClient srv.host srv.port $ \skt -> runInIO do
                spawnThread_ $ forever do
                    buf <- recvBinary client
                    liftIO (Network.Socket.ByteString.sendAll skt buf)

                forever do
                    buf <- liftIO (Network.Socket.ByteString.recv skt 65535)
                    atomically $ sendBinary client (from buf)

    forever do
        atomically (readPipe pipeAE) >>= \case
            AppDisplay (UserConnected "htmx" client) -> atomically (sendHtml client draw)
            AppDisplay (UserConnected "novnc" client) -> client.process.scope `Ki.fork_` onClient client
            _ -> pure ()

vncClient :: WinID -> (Int, Int) -> Text
vncClient wid (width, height) =
    [raw|
import RFB from '/xstatic/noVNC.js';
if (typeof butlerVNCs === "undefined") {
  globalThis.butlerVNCs = {}
}
globalThis.rfbTimer = undefined
function startRfb(wid, width, height) {
  if (butlerVNCs[wid] !== undefined && butlerVNCs[wid].rfbTimer !== undefined) {
      clearTimeout(butlerVNCs[wid].rfbTimer);
      butlerVNCs[wid].rfbTimer = undefined;
  }
  let rfb = new RFB(document.getElementById('vnc-' + wid), wsUrl("novnc"));
  butlerVNCs[wid] = rfb;
  rfb.qualityLevel = 0; rfb.compressionLevel = 9;
  // rfb.scaleViewport = true;
  // rfb.resizeSession = true;

  rfb.addEventListener("disconnect", e => {
    // reconnect
    butlerVNCs[wid].rfbTimer = setTimeout(() => {startRfb(wid)}, 1000)
    console.log("rfb disconnected", e);
  })

  rfb.addEventListener("clipboard", e => {
    console.log("vnc clipboard", e)
    navigator.clipboard.writeText(e.detail.text)
  })

  rfb.addEventListener("bell", e => {
    console.log("vnc bell", e)
  })

  rfb.focus()
  globalThis.rfb = rfb;
  let win = windows[wid];
  win.resize(width, height + 30);
  win.removeClass("no-resize")
     .removeClass("no-max")
     .removeClass("no-full");
}
|]
        <> "\n"
        <> ("startRfb" <> showT (wid, width, height))
