module Butler.Window where

import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Lucid

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
    , apps :: MemoryVar (Map AppID ProgramName)
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

addWindowApp :: WindowManager -> AppID -> Process -> STM ()
addWindowApp wm wid process = do
    modifyMemoryVar wm.apps (Map.insert wid process.program)
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
        with div_ [id_ ("w-" <> showT wid)] mempty
    with (script_ $ Text.intercalate ";" script) [type_ "module"]

windowScript :: AppID -> Text
windowScript wid =
    [raw|
import WinBox from '/xstatic/winbox.js'
function setupWindowManager(chan) {
  globalThis.WinBox = WinBox

  // WinBox event handler, called by the js client, forwarded to the server.
  globalThis.onWinEvent = (ev, w) => debounceData(500, (x, y) => {
    if (ev == "resize" && onWindowResize[w] !== undefined) {
      onWindowResize[w]()
    }
    return encodeDataMessage(chan, {ev: ev, w: w, x: x, y: y})
  })

  // Special handler for close event by the js client.
  globalThis.onWinClose = (w) => (force) => {
    let doDelete = force
    if (!force && confirm("Close window?")) {
      butlerDataSocket.send(encodeDataMessage(chan, {ev: "close", w: w}))
      doDelete = true
    }
    if (doDelete) {
      let div = document.getElementById("w-" + w);
      if (div) { div.remove(); }
      return false;
    }
    return true
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
renderWindow (idx, Window (x, y) (w, h) title) = do
    let attr k v = k <> ": " <> showT v
        attrs =
            [ attr "x" x
            , attr "y" y
            , attr "width" w
            , attr "height" h
            , "id: \"win-" <> showT idx <> "\""
            , "bottom: 36"
            , "class: [\"no-full\"]"
            , "root: document.getElementById(\"win-root\")"
            , "onclose: onWinClose(" <> showT idx <> ")"
            , "mount: document.getElementById(\"w-" <> showT idx <> "\")"
            ]
        obj = "{" <> Text.intercalate ",\n" attrs <> "}"
        win = "windows[" <> showT idx <> "]"
        mkObj = win <> " = (new WinBox(" <> from (show title) <> ", " <> obj <> "))"

        rs = "if (onWindowResize[" <> showT idx <> "]) {onWindowResize[" <> showT idx <> "]" <> showT (w, h) <> "}"

        handler n = win <> "[\"on" <> n <> "\"] = onWinEvent(" <> showT n <> ", " <> showT idx <> ")"
        handlers = map handler ["resize", "move", "focus"]
     in Text.intercalate ";\n" ([mkObj, rs] <> handlers) <> ";\n"
