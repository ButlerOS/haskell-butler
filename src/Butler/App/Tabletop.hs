module Butler.App.Tabletop where

import Data.Aeson
import Data.Text qualified as Text
import Lucid.Svg (SvgT, circle_, cx_, cy_, d_, defs_, fill_, offset_, path_, r_, radialGradient_, stop_, stop_color_, svg11_, transform_, version_)

import Butler
import Butler.Frame
import Butler.Logger
import Butler.NatMap qualified as NM
import Butler.Prelude

-- The data model
newtype OID = OID Natural
    deriving newtype (ToJSON)

newtype OName = OName Text
    deriving newtype (ToJSON)

data TableObject = TableObject
    { name :: OName
    , coord :: TVar (Word32, Word32)
    }

newtype TableState = TableState
    { tableObjects :: NM.NatMap TableObject
    }

newTableState :: STM TableState
newTableState = TableState <$> NM.newNatMap

newTableObject :: TableState -> OName -> STM OID
newTableObject ts name = do
    obj <- TableObject name <$> newTVar (42, 42)
    OID <$> NM.add ts.tableObjects obj

delTableObject :: TableState -> OID -> STM ()
delTableObject ts (OID oid) = NM.delete ts.tableObjects oid

setTableObject :: TableState -> OID -> (Word32, Word32) -> STM ()
setTableObject ts (OID oid) coord =
    NM.lookup ts.tableObjects oid >>= \case
        Nothing -> error ("Unknown object! " <> show oid)
        Just obj -> writeTVar obj.coord coord

getTableObjects :: TableState -> STM [(OID, TableObject)]
getTableObjects ts = fmap (first OID) <$> NM.elemsIndex ts.tableObjects

tableObjectJS :: (OID, TableObject) -> STM Text
tableObjectJS (OID oid, obj) = do
    (x, y) <- readTVar obj.coord
    pure $ "{oid: " <> showT oid <> ", x: " <> showT x <> ", y: " <> showT y <> "}"

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

renderTable :: TableState -> WinID -> ChannelID -> HtmlT STM ()
renderTable ts wid chan = with div_ [id_ (withWID wid "w")] do
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
    tableObjects <- lift (getTableObjects ts)
    forM_ tableObjects $ \(oid, obj) -> do
        with div_ [id_ (objID oid), class_ "cursor-pointer"] $ svg $ case obj.name of
            (OName "black-stone") -> blackStone
            (OName "white-stone") -> whiteStone
            (OName name) -> error $ "Unknown object " <> show name

    startingObjects <- lift (traverse tableObjectJS tableObjects)
    script_ (tabletopClient wid chan startingObjects)
  where
    -- This need to be kept in sync with the below js implementation 'mkElementID'
    objID (OID oid) = withWID wid ("oid-" <> showT oid)

-- Client events
data TableEvent
    = TableEventCreate OName -- {new: "name"}
    | TableEventMove OID Word32 Word32 -- {oid: 42, x: X, y: Y}
    | TableEventDelete OID -- {oid: 42}
    deriving (Generic, ToJSON)

instance FromJSON TableEvent where
    parseJSON = withObject "TableEvent" $ \obj -> do
        mOID <- fmap OID <$> (obj .:? "oid")
        mX <- obj .:? "x"
        case (mOID, mX) of
            (Nothing, Nothing) -> TableEventCreate . OName <$> obj .: "new"
            (Just oid, Nothing) -> pure (TableEventDelete oid)
            (Just oid, Just x) -> TableEventMove oid x <$> obj .: "y"
            _ -> fail "oid or x attribute missing"

tabletopApp :: WithDataEvents -> WithGuiEvents -> App
tabletopApp withDE withGE =
    App
        { name = "tabletop"
        , tags = fromList ["Game"]
        , description = "Free form tabletop"
        , size = Just (725, 696)
        , start = withDE (withGE . startTabletopApp)
        }

startTabletopApp :: DataEvents -> GuiEvents -> AppStart
startTabletopApp dataEvents guiEvents wid pipeDE = do
    tableState <- atomically newTableState

    let clientHandler :: DataEvent -> ProcessIO ()
        clientHandler de = case eitherDecode' (from de.buffer) of
            Right ev ->
                case ev of
                    TableEventCreate name -> do
                        newOID <- atomically (newTableObject tableState name)
                        logInfo "obj created" ["oid" .= newOID]
                        let msg = ["pending" .= newOID, "name" .= name]
                        sendsBinary dataEvents.clients (encodeMessageL dataEvents.chan (encodeJSON $ object msg))
                    TableEventMove oid x y -> do
                        -- logInfo "obj moved" ["ev" .= ev]
                        atomically $ setTableObject tableState oid (x, y)
                        sendsBinaryButSelf de.client dataEvents.clients (from de.rawBuffer)
                    TableEventDelete oid -> do
                        -- logInfo "obj removed" ["ev" .= ev]
                        atomically $ delTableObject tableState oid
                        sendsBinary dataEvents.clients (from de.rawBuffer)
            Left err -> logError "invalid json" ["ev" .= BSLog de.buffer, "err" .= err]

    forever do
        ev <- atomically (readPipe3 dataEvents.pipe guiEvents.pipe pipeDE)
        case ev of
            Left de -> clientHandler de
            Right (Left ge) -> logError "Unknown gui event" ["ev" .= ge]
            Right (Right de) -> sendHtmlOnConnect (renderTable tableState wid dataEvents.chan) de

tabletopClient :: WinID -> ChannelID -> [Text] -> Text
tabletopClient wid chan startingObjects =
    [raw|
function startTabletop(wid, chan, initialObjects, startingObjects) {
  // internal state
  const glButlerTT = { };

  const setFocus = (elt) => {
    if (glButlerTT.current && glButlerTT.current.id != elt.id) {
      glButlerTT.current.style.zIndex = 999;
    }
    glButlerTT.current = elt;
    elt.style.zIndex = 1000;
  }

  const mkElementID = (oid) => withWID(wid, "oid-" + oid);

  const trashElt = document.getElementById(withWID(wid, "trash-box"));
  const trashActivatedClass = "bg-pink-200";
  const isOverTrash = (pageX, pageY) => {
    const trashRect = trashElt.getBoundingClientRect();
    const in_width  = pageX + 5 > trashRect.left && pageX - 5 < trashRect.right;
    const in_height = pageY + 5 > trashRect.top && pageY - 5 < trashRect.bottom;
    return (in_width && in_height);
  };

  const winElt = document.getElementById(withWID(wid, "w"));

  const setPosition = (elt, x, y) => {
    elt.style.left = x + "px";
    elt.style.top = y + "px";
  }

  const newTableObject = (oid, name) => {
    const elt = document.getElementById(withWID(wid, name));

    // re-create the source object
    const srcElt = elt.cloneNode(true);
    srcElt.style.zIndex = 900;
    elt.parentNode.appendChild(srcElt);

    // setup the new object
    elt.butlerOID = oid;
    elt.id = mkElementID(oid);
    elt.style.position = 'absolute';
    winElt.appendChild(elt);

    // Attach handlers for the source object
    setupObject(srcElt);
  }

  // Handlers for event received by the server
  butlerDataHandlers[chan] = buf => {
    const body = decodeJSON(buf)
    console.log("Got server event", body);
    if (body.pending) {
      newTableObject(body.pending, body.name);
    } else if (body.oid) {
      const elt = document.getElementById(mkElementID(body.oid));
      if (elt === null) {
        console.error("Invalid OID", body);
        return;
      }
      if (body.x) {
        setFocus(elt);
        setPosition(elt, body.x, body.y);
      } else {
        elt.remove();
      }
    } else {
      console.error("Invalid table event", buf);
    }
  }

  const debounce = debounceData(100, ev =>
      encodeDataMessage(chan, ev)
  );

  // setupObject enables moving an element
  const setupObject = (elt) => {
    const clientMove = (x, y) => {
      setPosition(elt, x, y);
      debounce({oid: elt.butlerOID, x, y});
    }
    const clientDelete = () => {
      console.log("deleted", elt);
      butlerDataSocket.send(encodeDataMessage(chan, {oid: elt.butlerOID}))
    }

    const handler_mousedown = (event) => {
      // Ignore non-right click event
      if (event.buttons != 1) {
        return false;
      }

      if (initialObjects.includes(elt.dataset.name)) {
        butlerDataSocket.send(encodeDataMessage(chan, {new: elt.dataset.name}));
      }
      setFocus(elt);

      // Get position and dimention of the clicked element
      const boundingRect = elt.getBoundingClientRect()
      console.log({event, elt, boundingRect});

      const shiftX = event.clientX - boundingRect.left;
      const shiftY = event.clientY - boundingRect.top;

      const moveAt = (pageX, pageY) => {
        const winRect = winElt.getBoundingClientRect();
        const x = pageX - shiftX - winRect.left;
        const y = pageY - shiftY - winRect.top;

        const in_width = x > 0 && (x + boundingRect.width) < window.innerWidth;
        const in_height = y > 0 && (y + boundingRect.height) < window.innerHeight;
        if (in_width && in_height) {
          clientMove(x, y);
        }

        if (isOverTrash(pageX, pageY)) {
          trashElt.classList.add(trashActivatedClass);
        } else {
          trashElt.classList.remove(trashActivatedClass);
        }
      }

      // Move the element where it should be, in case it just got created
      moveAt(event.pageX, event.pageY);

      const handler_mousemove = (event) => {
        moveAt(event.pageX, event.pageY);
      }

      const handler_mouseup = (event) => {
        console.log({event, elt});
        if (trashElt.classList.contains(trashActivatedClass)) {
          clientDelete();
          trashElt.classList.remove(trashActivatedClass)
        }
        window.removeEventListener('mousemove', handler_mousemove);
        window.removeEventListener('mouseup', handler_mouseup);
      }

      window.addEventListener('mousemove', handler_mousemove);
      window.addEventListener('mouseup', handler_mouseup);
    };

    elt.addEventListener("mousedown", handler_mousedown)

    elt.ondragstart = function() {
      return false;
    };
  };

  const setupObjectID = (id) => setupObject(document.getElementById(withWID(wid, id)));

  initialObjects.forEach(setupObjectID);
  startingObjects.forEach(obj => {
    const elt = document.getElementById(mkElementID(obj.oid))
    setupObject(elt);
    elt.style.position = 'absolute';
    setPosition(elt, obj.x, obj.y);
  });
}
  |]
        <> "\nstartTabletop("
        <> Text.intercalate ", " initTableArgs
        <> ");"
  where
    initTableArgs =
        [ showT wid
        , showT chan
        , showT @[Text] ["white-stone", "black-stone"]
        , "[" <> Text.intercalate ", " startingObjects <> "]"
        ]
