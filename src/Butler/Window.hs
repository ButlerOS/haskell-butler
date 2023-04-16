module Butler.Window where

import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Lucid

import Butler.App (AppInstance (..), wid_)
import Butler.AppID
import Butler.Core
import Butler.Core.Memory
import Butler.Core.Process
import Butler.Prelude

data Window = Window
    { position :: (Int, Int)
    , size :: (Int, Int)
    , title :: Text
    }
    deriving (Eq, Generic, Serialise)

data WindowsState = WindowsState
    { windows :: Map AppID Window
    , focus :: Maybe AppID
    }
    deriving (Generic, Serialise)

type Windows = MemoryVar WindowsState

data WindowManager = WindowManager
    { windows :: MemoryVar WindowsState
    , apps :: MemoryVar (Map AppID (Maybe Value, ProgramName))
    }

newWindowManager :: ProcessIO WindowManager
newWindowManager =
    WindowManager
        <$> (snd <$> newProcessMemory "wins.bin" (pure newWindows))
        <*> (snd <$> newProcessMemory "apps.bin" (pure mempty))

getWindowIDs :: Windows -> STM [AppID]
getWindowIDs ws = do
    windowsState <- readMemoryVar ws
    pure $ Map.keys windowsState.windows

newWindows :: WindowsState
newWindows = WindowsState mempty Nothing

addWindowApp :: WindowManager -> AppInstance -> STM ()
addWindowApp wm appInstance = do
    let wid = appInstance.wid
        process = appInstance.process
    modifyMemoryVar wm.apps (Map.insert wid (appInstance.argv, process.program))
    Map.lookup wid . (.windows) <$> readMemoryVar wm.windows >>= \case
        Nothing -> void $ newWindow wm.windows wid (processID process)
        Just _win -> void $ updateWindow wm.windows wid (#title .~ processID process)

delWindowApp :: WindowManager -> AppID -> STM ()
delWindowApp wm wid = do
    deleteWindow wm.windows wid
    modifyMemoryVar wm.apps (Map.delete wid)

lookupWindow :: Windows -> AppID -> STM (Maybe Window)
lookupWindow ws wid = Map.lookup wid . (.windows) <$> readMemoryVar ws

deleteWindow :: Windows -> AppID -> STM ()
deleteWindow ws wid = modifyMemoryVar ws (#windows %~ Map.delete wid)

updateWindow :: Windows -> AppID -> (Window -> Window) -> STM Bool
updateWindow ws wid f = stateMemoryVar ws $ \s ->
    case Map.lookup wid s.windows of
        Nothing -> (False, s)
        Just win ->
            let newWin = f win
             in if newWin == win
                    then (False, s)
                    else (True, s & #windows %~ Map.insert wid newWin)

newWindow :: Windows -> AppID -> Text -> STM Window
newWindow ws appID title = stateMemoryVar ws $ \s ->
    let
        current = Map.size s.windows
        win =
            Window
                (0 + 23 * current, 0 + 23 * current)
                (640, 420)
                title
     in
        (win, s & (#windows %~ Map.insert appID win))

renderWindows :: AppID -> Windows -> HtmlT STM ()
renderWindows controlWID ws = do
    w <- lift (readMemoryVar ws)
    let windows :: [(AppID, Window)]
        windows = Map.toList w.windows
        createWindows = map renderWindow windows
        script = windowScript controlWID : createWindows
    forM_ (fst <$> windows) \wid ->
        with div_ [wid_ wid "w"] mempty
    with (script_ $ Text.intercalate ";\n" script) [type_ "module"]

windowScript :: AppID -> Text
windowScript wid =
    [raw|
import WinBox from '/xstatic/winbox.js'
function setupWindowManager(chan) {
  // WinBox event handler, called by the js client, forwarded to the server.
  const onWinEvent = (ev, w) => debounceData(500, (x, y) => {
    if (ev == "resize" && onWindowResize[w] !== undefined) {
      onWindowResize[w]()
    }
    sendJSONMessage(chan, {ev: ev, w: w, x: x, y: y})
  })

  // Special handler for close event by the js client.
  const onWinClose = (w) => (force) => {
    let doDelete = force
    if (!force) {
      sendJSONMessage(chan, {ev: "close", w: w})
      // Don't delete the window right away, let's wait for ack from server
      doDelete = false
    }
    if (doDelete) {
      let div = document.getElementById("w-" + w);
      if (div) { div.remove(); }
      return false;
    }
    return true
  }

  // Handle win creation
  globalThis.renderWindow = (wid, title, x, y, width, height) => {
    windows[wid] = new WinBox(title, {x,y,width,height,
      id: withWID(wid, "win"),
      bottom: 36,
      class: ["no-full"],
      root: document.getElementById("win-root"),
      onclose: onWinClose(wid),
      mount: document.getElementById(withWID(wid, "w"))});
    if (onWindowResize[wid]) {onWindowResize[wid](width,height)};
    windows[wid]["onresize"] = onWinEvent("resize", wid);
    windows[wid]["onmove"] = onWinEvent("move", wid);
    windows[wid]["onfocus"] = onWinEvent("focus", wid);
  }

  // Servent event handler.
  butlerDataHandlers[chan] = buf => {
    let body = decodeJSON(buf)
    let win = windows[body.w]
    let withoutHandler = (name, cb) => {
      // disable the handler to avoid bouncing loop effect
      let handler = win[name]
      win[name] = undefined
      cb()
      win[name] = handler
    }
    switch (body.ev) {
      case "move":
        withoutHandler("onmove", () => win.move(body.x, body.y))
        break
      case "resize":
        withoutHandler("onresize", () => {
          if (onWindowResize[body.w] !== undefined) {
            onWindowResize[body.w](body.x, body.y)
          }
          win.resize(body.x, body.y)
        })
        break
      case "focus":
        withoutHandler("onfocus", () => win.focus(true))
        break
      case "close":
        win.close(true)
        break
      case "title":
        win.setTitle(body.title)
        break
    }
  }
}
|]
        <> "\nsetupWindowManager("
        <> showT wid
        <> ");"

renderWindow :: (AppID, Window) -> Text
renderWindow (wid, Window (x, y) (w, h) title) =
    "renderWindow" <> showT (wid, title, x, y, w, h)
