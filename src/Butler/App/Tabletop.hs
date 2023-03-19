module Butler.App.Tabletop where

import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Lucid.Svg (SvgT, circle_, cx_, cy_, d_, defs_, fill_, offset_, path_, r_, radialGradient_, stop_, stop_color_, svg11_, transform_, version_)

import Butler
import Butler.Core.NatMap qualified as NM
import Butler.Frame

-- The data model
newtype OName = OName Text
    deriving newtype (IsString, Eq, ToJSON)

instance From OName Text where
    from (OName n) = n

data TableObject = TableObject
    { elt :: OName
    , coord :: TVar (Word, Word)
    }

data TableState = TableState
    { players :: TVar (Map Endpoint (Either OName TableObject))
    , objects :: NM.NatMap TableObject
    }

isObject :: OName -> Maybe Natural
isObject (OName name) = case decodeTriggerName name of
    Just (WinID n, _) -> Just (unsafeFrom n)
    Nothing -> Nothing

newTableState :: STM TableState
newTableState = TableState <$> newTVar mempty <*> NM.newNatMap

newTableObject :: TableState -> OName -> STM TableObject
newTableObject ts (OName name) = do
    let mkObj :: Natural -> STM TableObject
        mkObj n = TableObject (OName $ name <> "-" <> showT n) <$> newTVar (42, 42)
    NM.addWithKey ts.objects mkObj

-- Html rendering
type SVG = SvgT STM ()

svg :: SVG -> SVG
svg content = do
    with (svg11_ content) [version_ "1.1", width_ "30", height_ "30"]

mkStone :: Text -> SVG -> SVG
mkStone gid gradient = do
    defs_ gradient
    circle_ [cx_ "15", cy_ "15", r_ "15", fill_ [fmt|url(#{gid})|]]

trashBin :: SVG
trashBin =
    path_
        [ transform_ "scale(1.4)"
        , d_ "M17 6h5v2h-2v13a1 1 0 0 1-1 1H5a1 1 0 0 1-1-1V8H2V6h5V3a1 1 0 0 1 1-1h8a1 1 0 0 1 1 1v3zm1 2H6v12h12V8zm-9 3h2v6H9v-6zm4 0h2v6h-2v-6zM9 4v2h6V4H9z"
        ]

blackStone :: SVG
blackStone = mkStone "$brg" do
    with radialGradient_ [id_ "$brg", cx_ ".3", cy_ ".3", r_ ".8"] do
        stop_ [offset_ "0", stop_color_ "#777"]
        stop_ [offset_ ".3", stop_color_ "#222"]
        stop_ [offset_ "1", stop_color_ "#000"]

whiteStone :: SVG
whiteStone = mkStone "$wrg" do
    with radialGradient_ [id_ "$wrg", cx_ ".47", cy_ ".49", r_ ".48"] do
        stop_ [offset_ ".7", stop_color_ "#FFF"]
        stop_ [offset_ ".9", stop_color_ "#DDD"]
        stop_ [offset_ "1", stop_color_ "#777"]

renderTable :: WinID -> HtmlT STM ()
renderTable wid = with div_ [id_ (withWID wid "w")] do
    void $ with div_ [class_ "flex flex-row"] do
        with div_ [class_ "flex flex-col my-6"] do
            with div_ [class_ "border bg-gray-200 rounded p-1 m-2"] do
                with div_ [id_ (withWID wid "black-stone"), data_ "name" "black-stone", class_ "cursor-pointer"] (svg blackStone)
            with div_ [class_ "border bg-gray-200 rounded p-1 m-2"] do
                with div_ [id_ (withWID wid "white-stone"), data_ "name" "white-stone", class_ "cursor-pointer"] (svg whiteStone)
            with div_ [class_ "grow"] mempty
            with div_ [id_ (withWID wid "trash-box"), class_ "m-2"] (svg trashBin)
        with div_ [class_ "m-6"] do
            replicateM 19 do
                with div_ [class_ "flex flex-row"] do
                    replicateM 19 do
                        with div_ [class_ "w-8 h-8 border"] mempty
    script_ (tabletopClient wid)

-- Client events
data TableEvent
    = TableEventClick Text
    | TableEventMove Word Word
    | TableEventDelete Text
    deriving (Generic, ToJSON)

instance FromJSON TableEvent where
    parseJSON = withObject "TableEvent" $ \obj -> do
        let clickParser = TableEventClick . dropWID <$> obj .: "click"
            deleteParser = TableEventDelete . dropWID <$> obj .: "del"
            moveParser = TableEventMove <$> obj .: "x" <*> obj .: "y"
        clickParser <|> deleteParser <|> moveParser

tabletopApp :: App
tabletopApp =
    (defaultApp "tabletop" startTabletopApp)
        { tags = fromList ["Game"]
        , description = "Free form tabletop"
        , size = Just (725, 696)
        }

startTabletopApp :: AppContext -> ProcessIO ()
startTabletopApp ctx = do
    tableState <- atomically newTableState

    let newObject :: OName -> OName -> LByteString
        newObject src elt =
            let msg = object ["new" .= elt, "src" .= src]
             in encodeMessageL ctx.wid (encodeJSON msg)

        moveObject :: OName -> (Word, Word) -> LByteString
        moveObject elt (x, y) =
            let msg = object ["elt" .= elt, "x" .= x, "y" .= y]
             in encodeMessageL ctx.wid (encodeJSON msg)

        delObject :: OName -> LByteString
        delObject elt =
            let msg = object ["del" .= elt]
             in encodeMessageL ctx.wid (encodeJSON msg)

    let clientHandler :: DisplayClient -> TableEvent -> ProcessIO ()
        clientHandler client = \case
            TableEventClick (OName -> name) -> do
                mValue <- case isObject name of
                    Just k -> fmap Right <$> atomically (NM.lookup tableState.objects k)
                    Nothing -> pure $ Just $ Left name
                case mValue of
                    Nothing -> logError "Unknown name" ["name" .= name]
                    Just value -> atomically do
                        modifyTVar' tableState.players $ Map.insert client.endpoint value
            TableEventMove x y ->
                atomically (Map.lookup client.endpoint <$> readTVar tableState.players) >>= \case
                    Just eObj -> do
                        mTableObject <- case eObj of
                            Right obj -> pure (Just obj)
                            Left name
                                | name `elem` ["black-stone", "white-stone"] -> do
                                    obj <- atomically $ newTableObject tableState name
                                    atomically $ modifyTVar' tableState.players $ Map.insert client.endpoint (Right obj)
                                    sendsBinary ctx.clients $ newObject name obj.elt
                                    pure (Just obj)
                                | otherwise -> pure Nothing
                        case mTableObject of
                            Just obj -> do
                                atomically (writeTVar obj.coord (x, y))
                                sendsBinary ctx.clients $ moveObject obj.elt (x, y)
                            Nothing -> logError "Unknown object" []
                    Nothing -> logError "Player doesn't own an object" []
            TableEventDelete (OName -> name) -> case isObject name of
                Just k -> do
                    isDeleted <- atomically do
                        NM.lookup tableState.objects k >>= \case
                            Just _obj -> do
                                NM.delete tableState.objects k
                                pure True
                            Nothing -> pure False
                    if isDeleted
                        then do
                            logDebug "Deleted" ["name" .= name]
                            sendsBinary ctx.clients $ delObject name
                        else logError "Unknown obj" ["name" .= name]
                Nothing -> logError "Invalid obj" ["name" .= name]

    forever do
        atomically (readPipe ctx.pipe) >>= \case
            AppDisplay (UserConnected "htmx" client) -> do
                atomically $ sendHtml client $ renderTable ctx.wid
                objs <- atomically (NM.elems tableState.objects)
                forM_ objs \obj -> do
                    atomically $ sendBinary client $ newObject (OName $ dropWID $ from $ obj.elt) obj.elt
                    atomically . sendBinary client . moveObject obj.elt =<< readTVarIO obj.coord
            AppDisplay (UserDisconnected "htmx" client) -> do
                atomically $ modifyTVar' tableState.players (Map.delete client.endpoint)
            AppData ev -> case decodeJSON @TableEvent (from ev.buffer) of
                Just tableEvent -> clientHandler ev.client tableEvent
                Nothing -> logError "Unknown event" ["ev" .= ev]
            _ -> pure ()

tabletopClient :: WinID -> Text
tabletopClient wid =
    [raw|
function startTabletop(wid, initialObjects) {
  // internal state
  const glTT = { current: null };

  // setup trash box handler
  const trashElt = document.getElementById(withWID(wid, "trash-box"));
  const trashActivatedClass = "bg-pink-200";
  const updateTrash = (pageX, pageY) => {
    const trashRect = trashElt.getBoundingClientRect();
    const in_width  = pageX + 5 > trashRect.left && pageX - 5 < trashRect.right;
    const in_height = pageY + 5 > trashRect.top && pageY - 5 < trashRect.bottom;
    if (in_width && in_height) {
      glTT.trashed = true
      trashElt.classList.add(trashActivatedClass)
    } else {
      glTT.trashed = false
      trashElt.classList.remove(trashActivatedClass)
    }
  };

  // setup window mouse handler
  const winElt = document.getElementById(withWID(wid, "w"));
  winElt.onmousemove = debounceData(100, event => {
    if (glTT.current === null) {
      return null
    }
    // Calculate the object position relative to the app window
    const winRect = winElt.getBoundingClientRect();
    const x = event.pageX - glTT.shiftX - winRect.left;
    const y = event.pageY - glTT.shiftY - winRect.top;

    // Make sure the event is inside the window
    const in_width = x > 0 && (x + glTT.boundingRect.width) < window.innerWidth;
    const in_height = y > 0 && (y + glTT.boundingRect.height) < window.innerHeight;

    if (in_width && in_height) {
      const moveEv = {x, y}
      updateTrash(event.pageX, event.pageY)
      return encodeDataMessage(wid, moveEv)
    }
  })
  winElt.onmouseup = event => {
    if (glTT.current === null) {
      return null
    }
    if (glTT.trashed) {
      const delEv = {del: glTT.current.id}
      console.log(delEv)
      butlerDataSocketSend(encodeDataMessage(wid, delEv))
    }
    updateTrash(0, 0)
    glTT.current = null
  }

  // setupObject enables moving an element
  const setupObject = (elt) => {
    elt.onmousedown = (event) => {
      // Ignore non-left click event
      if (event.buttons != 1) {
        return false;
      }
      // Record object information for mousemouve handler
      glTT.boundingRect = elt.getBoundingClientRect()
      glTT.shiftX = event.clientX - glTT.boundingRect.left
      glTT.shiftY = event.clientY - glTT.boundingRect.top
      glTT.current = elt
      const clickEv = {click: glTT.current.id}
      // Tell the server
      butlerDataSocketSend(encodeDataMessage(wid, clickEv))
    };
    elt.ondragstart = function() {
      return false;
    };
  };

  // Handlers for event received by the server
  const setFocus = (elt) => {
    if (glTT.focus && glTT.focus.id != elt.id) {
      glTT.focus.style.zIndex = 999;
    }
    glTT.focus = elt;
    elt.style.zIndex = 1000;
  }
  const setPosition = (elt, x, y) => {
    elt.style.left = x + "px";
    elt.style.top = y + "px";
  }
  butlerDataHandlers[wid] = buf => {
    const body = decodeJSON(buf)
    console.log("Got server event", body)
    if (body.new) {
      const srcElt = document.getElementById(withWID(wid, body.src));

      // re-create the source object
      const elt = srcElt.cloneNode(true)
      elt.style.zIndex = 900

      // setup the new object
      elt.id = withWID(wid, body.new)
      elt.style.position = 'absolute'
      setPosition(elt, 42, 100)
      winElt.appendChild(elt)
      glTT.current = elt

      // Attach handlers for the source object
      setupObject(elt);
    } else if (body.elt) {
      const elt = document.getElementById(withWID(wid, body.elt))
      if (elt === null) {
        console.error("Invalid OID", body);
        return;
      }
      setFocus(elt);
      setPosition(elt, body.x, body.y);
    } else if (body.del) {
      document.getElementById(withWID(wid, body.del)).remove()
    } else {
      console.error("Invalid table event", buf);
    }
  }

  initialObjects.forEach(id =>
    setupObject(document.getElementById(withWID(wid, id)))
  )
}
  |]
        <> "\nstartTabletop("
        <> Text.intercalate ", " initTableArgs
        <> ");"
  where
    initTableArgs =
        [ showT wid
        , showT @[Text] ["white-stone", "black-stone"]
        ]
