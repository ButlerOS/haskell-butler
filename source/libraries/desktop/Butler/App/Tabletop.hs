module Butler.App.Tabletop (tabletopApp) where

import Data.IntSet qualified as IS
import Data.Text qualified as Text
import Lucid.Svg (circle_, cx_, cy_, d_, defs_, fill_, offset_, path_, r_, radialGradient_, stop_, stop_color_, svg11_, transform_, version_)

import Butler
import Butler.Core.NatMap qualified as NM
import Butler.Database
import Butler.Frame

import Butler.App.Chess

tabletopApp :: App
tabletopApp =
    (defaultApp "tabletop" startTabletopApp)
        { tags = fromList ["Game"]
        , description = "Free form tabletop"
        , size = Just (1021, 656)
        }

{- | The name of an object.
The initial object don't have a suffix id, e.g. "white-stone".
Clicking on an initial object create a copy with an object id suffix, e.g. "white-stone-1".
-}
newtype OName = OName Text
    deriving newtype (IsString, Eq, FromJSON, ToJSON, FromField, ToField)

{- | Get an object id
>>> (OName "test-1").oid
Just 1
>>> (OName "white-sone").oid
Nothing
-}
instance HasField "oid" OName (Maybe Natural) where
    getField (OName name) = case decodeNaturalSuffix name of
        Just (n, _) -> Just n
        Nothing -> Nothing

instance From OName Text where from (OName n) = n

-- | An object on the table
data TableObject = TableObject
    { elt :: OName
    , coord :: TVar (Word, Word)
    }

-- | The 'TableState' is the current game on display.
data TableState = TableState
    { current :: Game
    , objects :: NM.NatMap TableObject
    , dirties :: TVar IntSet
    }
    deriving (Generic)

newTableState :: Game -> STM TableState
newTableState game = TableState game <$> NM.newNatMap <*> newTVar mempty

data GameStatus = Selecting | Playing TableState

data GameKind
    = Baduk Natural
    | Chess
    | Checker

-- | Inital game object drawing definition
data GameObject = GameObject
    { name :: Text
    , draw :: HtmlT STM ()
    }

-- | The initial game object
gameObjects :: GameKind -> [[GameObject]]
gameObjects = \case
    Baduk{} ->
        [ [GameObject "black-stone" (svg blackStone)]
        , [GameObject "white-stone" (svg whiteStone)]
        ]
    Chess ->
        [
            [ GameObject "black-king" blackKing
            , GameObject "black-queen" blackQueen
            , GameObject "black-rook" blackRook
            , GameObject "black-bishop" blackBishop
            , GameObject "black-knight" blackKnight
            , GameObject "black-pawn" blackPawn
            ]
        ,
            [ GameObject "white-king" whiteKing
            , GameObject "white-queen" whiteQueen
            , GameObject "white-rook" whiteRook
            , GameObject "white-bishop" whiteBishop
            , GameObject "white-knight" whiteKnight
            , GameObject "white-pawn" whitePawn
            ]
        ]
    Checker ->
        -- TODO: add checker pieces
        []

gameObjectsNames :: GameKind -> [Text]
gameObjectsNames kind = do
    objs <- gameObjects kind
    obj <- objs
    pure obj.name

instance From GameKind Text where
    from = \case
        Baduk n -> encodeNaturalSuffix n "baduk"
        Chess -> "chess"
        Checker -> "checker"

instance TryFrom Text GameKind where
    tryFrom = maybeTryFrom decodeGameKindText

decodeGameKindText :: Text -> Maybe GameKind
decodeGameKindText name = case (name, decodeNaturalSuffix name) of
    ("chess", _) -> Just Chess
    ("checker", _) -> Just Checker
    (_, Just (count, "baduk")) -> Just (Baduk count)
    _ -> Nothing

data Game = Game
    { kind :: GameKind
    , name :: Maybe (GameID, Text)
    }

gameMatch :: GameID -> Game -> Bool
gameMatch gameID game = case game.name of
    Just (gid, _) -> gid == gameID
    Nothing -> False

newtype GameID = GameID Int64
    deriving newtype (Eq, ToJSON, FromJSON, ToField, FromField)

-- | The main application state
data AppState = AppState
    { status :: GameStatus
    , games :: [Game]
    }
    deriving (Generic)

-- | The database stores the saved game and the object positions
tabletopDatabase :: DatabaseMigration
tabletopDatabase = DatabaseMigration ["games-create", "objects-create"] doUp doDown
  where
    doUp name db = case name of
        "games-create" -> dbExecute db "CREATE TABLE games (id INTEGER PRIMARY KEY, name TEXT, game TEXT)" []
        "objects-create" -> dbExecute db "CREATE TABLE objects (gameid INTEGER, name TEXT, x INTEGER, y INTEGER)" []
        _ -> pure ()
    doDown _ _ = pure ()

-- | Load the saved game from the database
gamesFromDB :: Database -> ProcessIO [Either Text Game]
gamesFromDB db = fmap mkGame <$> dbQuery db "select * from games" []
  where
    mkGame (gameID, name, gameName) =
        case tryFrom @Text gameName of
            Right kind -> Right $ Game kind (Just (gameID, name))
            Left (TryFromException e _) -> Left e

-- | Load a game from the database
tableStateFromDB :: Database -> Game -> ProcessIO TableState
tableStateFromDB db game = do
    ts <- atomically (newTableState game)
    forM_ (game.name) (loadObjects ts . fst)
    pure ts
  where
    loadObjects ts gameID = do
        objs <- traverse toObject =<< dbQuery db "SELECT name,x,y FROM objects WHERE gameid = :id" [":id" := gameID]
        forM_ objs \obj -> case obj.elt.oid of
            Nothing -> logError "Invalid obj from db" ["name" .= obj.elt]
            Just k -> atomically (NM.insert ts.objects k obj)

    toObject (name, x, y) = TableObject (OName name) <$> newTVarIO (x, y)

-- | Update an object position
replaceObjectDB :: Database -> GameID -> (OName, Word, Word) -> ProcessIO ()
replaceObjectDB db gameID (name, x, y) = do
    updateCount <-
        dbUpdate
            db
            "UPDATE objects SET x = :x, y = :y WHERE name = :name AND gameid = :gameid "
            [":x" := x, ":y" := y, ":name" := name, ":gameid" := gameID]
    when (updateCount == 0) do
        insertObjectDB db gameID (name, x, y)

-- | Add an object
insertObjectDB :: Database -> GameID -> (OName, Word, Word) -> ProcessIO ()
insertObjectDB db gameID (name, x, y) = do
    dbExecute
        db
        "INSERT INTO objects (gameid, name, x, y) VALUES (:gameid, :name, :x, :y)"
        [":gameid" := gameID, ":name" := name, ":x" := x, ":y" := y]

-- | Delete an object
deleteObjectDB :: Database -> GameID -> OName -> ProcessIO ()
deleteObjectDB db gameID name = do
    dbExecute db "DELETE FROM objects where gameid = :gameid AND name = :name" [":gameid" := gameID, ":name" := name]

-- | Initial the main app state
newAppState :: Database -> ProcessIO AppState
newAppState db = do
    (errors, games) <- partitionEithers <$> gamesFromDB db
    when (errors /= []) do
        logError "Tabletop save loading error" ["errors" .= errors]
    pure $ AppState Selecting games

-- | Create a new object
newTableObject :: TableState -> OName -> STM TableObject
newTableObject ts (OName name) = do
    let mkObj :: Natural -> STM TableObject
        mkObj n = TableObject (OName $ name <> "-" <> showT n) <$> newTVar (42, 42)
    NM.addWithKey ts.objects mkObj

-- Html rendering
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

renderTable :: AppID -> GameKind -> HtmlT STM ()
renderTable wid kind = do
    void $ with div_ [class_ "flex flex-row flex-grow"] do
        with div_ [class_ "flex flex-col my-6"] do
            with div_ [class_ "flex flex-row gap-2"] do
                forM_ (gameObjects kind) \xs -> do
                    with div_ [class_ "flex flex-col"] do
                        forM_ xs \gameObject -> do
                            with div_ [class_ "border bg-gray-200 rounded p-1 m-2"] do
                                with div_ [wid_ wid gameObject.name, class_ "cursor-pointer"] gameObject.draw
            with div_ [class_ "grow"] mempty
            with div_ [wid_ wid "trash-box", class_ "m-2"] (svg trashBin)
        with div_ [class_ "m-6"] do
            case kind of
                Baduk (unsafeFrom -> size) -> void do
                    replicateM (size - 1) do
                        with div_ [class_ "flex flex-row"] do
                            replicateM (size - 1) do
                                with div_ [class_ "w-8 h-8 border"] mempty
                _ -> board
    script_ (tabletopClient wid kind)

renderCurrentGame :: AppID -> Maybe TableState -> Text -> HtmlT STM ()
renderCurrentGame wid mTableState name = with div_ [wid_ wid "current-game"] do
    toHtml ("Playing: " <> name)
    isClean <- case mTableState of
        Nothing -> pure True
        Just tableState -> IS.null <$> lift (readTVar tableState.dirties)
    unless isClean do
        " *"

renderLoader :: AppState -> AppID -> HtmlT STM ()
renderLoader appState wid = with div_ [wid_ wid "tabletop-game-list", class_ "flex flex-col gap-4"] do
    "Game List"
    mTS <- case appState.status of
        Playing ts -> do
            withTrigger_ "" wid "save-game" (input_ []) [type_ "text", name_ "name", placeholder_ "New Save game"]
            pure (Just ts)
        _ -> pure Nothing
    let mGameID = (\ts -> fst <$> ts.current.name) =<< mTS
    with div_ [class_ "flex-grow"] do
        forM_ appState.games \game -> case game.name of
            Just (gameID, name) ->
                div_ [class_ "flex border"] do
                    if Just gameID == mGameID
                        then do
                            renderCurrentGame wid mTS name
                        else do
                            withTrigger "click" wid "load-game" ["id" .= gameID] button_ [class_ "mx-2 flex-grow"] (toHtml $ name <> " (" <> into @Text game.kind <> ")")
                            withTrigger "click" wid "del-game" ["id" .= gameID] i_ [class_ "ri-delete-bin-2-fill text-red-500 mx-2 cursor-pointer", title_ "Delete the game"] mempty
            Nothing -> error "Game without ID must not be added to the saved list"
    withTrigger_ "click" wid "new-game" button_ [class_ btnGreenClass] "New Game"

renderSelector :: AppID -> HtmlT STM ()
renderSelector wid = with div_ [class_ "flex-grow"] do
    "Select a game"
    ul_ do
        let gameButton name kind = li_ do
                withTrigger "click" wid "start-game" ["kind" .= into @Text kind] button_ [class_ "mx-4 my-2 border"] name
        gameButton "Baduk 19" (Baduk 19)
        gameButton "Baduk 13" (Baduk 13)
        gameButton "Baduk 9" (Baduk 9)
        gameButton "Chess" Chess
        gameButton "Checker" Checker

mountUI :: AppState -> AppID -> HtmlT STM ()
mountUI appState wid = with div_ [wid_ wid "w", class_ "flex flex-row gap-2"] do
    case appState.status of
        Selecting{} -> renderSelector wid
        Playing game -> renderTable wid game.current.kind
    renderLoader appState wid

-- | Client events
data TableEvent
    = -- | The client clicked an object
      TableEventClick OName
    | -- | The client moved an object
      TableEventMove OName Word Word
    | -- | The client dropped an object on the trash
      TableEventDelete OName
    deriving (Generic, ToJSON)

instance FromJSON TableEvent where
    parseJSON = withObject "TableEvent" $ \obj -> do
        let clickParser = TableEventClick <$> obj .: "click"
            deleteParser = TableEventDelete <$> obj .: "del"
            moveParser = TableEventMove <$> obj .: "n" <*> obj .: "x" <*> obj .: "y"
        clickParser <|> deleteParser <|> moveParser

startTabletopApp :: AppContext -> ProcessIO ()
startTabletopApp ctx = withDatabase "tabletop" tabletopDatabase \db -> do
    tAppState <- newTVarIO =<< newAppState db

    -- Server event encoding helper
    let newObject :: OName -> OName -> Bool -> LByteString
        newObject src elt grab =
            let msg = object ["new" .= elt, "src" .= src, "grab" .= grab]
             in encodeMessage (from ctx.wid) (encodeJSON msg)

        moveObject :: OName -> (Word, Word) -> LByteString
        moveObject elt (x, y) =
            let msg = object ["elt" .= elt, "x" .= x, "y" .= y]
             in encodeMessage (from ctx.wid) (encodeJSON msg)

        delObject :: OName -> LByteString
        delObject elt =
            let msg = object ["del" .= elt]
             in encodeMessage (from ctx.wid) (encodeJSON msg)

    let updateDirty tableState =
            case tableState.current.name of
                Just (_, name') ->
                    -- Update the current game UI to indicate it is dirty.
                    sendsHtml ctx.shared.clients $ renderCurrentGame ctx.wid (Just tableState) name'
                _ -> pure ()

    let clientHandler :: TableState -> DisplayClient -> TableEvent -> ProcessIO ()
        clientHandler tableState client = \case
            TableEventClick name
                | coerce name `elem` gameObjectsNames tableState.current.kind -> do
                    -- Add the object to the tableState.
                    obj <- atomically $ newTableObject tableState name

                    -- Add the object to the client hand.
                    atomically $ sendBinary client $ newObject name obj.elt True
                    -- Tell the others a new object has been created.
                    sendsBinaryButSelf client ctx.shared.clients $ newObject name obj.elt False
                | otherwise -> logError "Unknown object" ["name" .= name]
            TableEventMove name x y -> case name.oid of
                Just k ->
                    atomically (NM.lookup tableState.objects k) >>= \case
                        Just obj -> do
                            isNewDirty <- atomically do
                                -- update the object coordinate.
                                writeTVar obj.coord (x, y)
                                -- add to the list of dirty object.
                                stateTVar tableState.dirties \dirties ->
                                    (IS.null dirties, IS.insert (unsafeFrom @Natural @Int k) dirties)
                            when isNewDirty $ updateDirty tableState
                            -- Tell the clients an object moved.
                            sendsBinaryButSelf client ctx.shared.clients $ moveObject obj.elt (x, y)
                        Nothing -> logError "Unknown obj" ["name" .= name]
                Nothing -> logError "Invalid obj" ["name" .= name]
            TableEventDelete name -> case name.oid of
                Just k -> do
                    (isNewDirty, isDeleted) <- atomically do
                        NM.lookup tableState.objects k >>= \case
                            Just _obj -> do
                                NM.delete tableState.objects k
                                stateTVar tableState.dirties \dirties ->
                                    ((IS.null dirties, True), IS.delete (unsafeFrom k) dirties)
                            Nothing -> pure (False, False)
                    if isDeleted
                        then do
                            logDebug "Deleted" ["name" .= name]
                            sendsBinary ctx.shared.clients $ delObject name
                            when isNewDirty $ updateDirty tableState
                            forM_ tableState.current.name \(gameID, _) -> deleteObjectDB db gameID name
                        else logError "Unknown obj" ["name" .= name]
                Nothing -> logError "Invalid obj" ["name" .= name]

    let updateState :: (AppState -> AppState) -> STM AppState
        updateState up = stateTVar tAppState \prev ->
            let new = up prev in (new, new)

    let _rename :: Text -> GameID -> ProcessIO ()
        _rename name gid = dbExecute db "UPDATE games SET name = :name where id = :id" [":name" := name, ":id" := gid]

    let refreshUI :: AppState -> (LByteString -> ProcessIO ()) -> ProcessIO ()
        refreshUI appState send = do
            case appState.status of
                Playing tableState -> do
                    objs <- atomically (NM.elems tableState.objects)
                    forM_ objs \obj -> do
                        send $ newObject (OName $ dropWID $ from $ obj.elt) obj.elt False
                        send . moveObject obj.elt =<< readTVarIO obj.coord
                _ -> pure ()

    let collectDirties = do
            appState <- readTVar tAppState
            case appState.status of
                Playing tableState -> case tableState.current.name of
                    Just (gameID, name) -> do
                        dirties <- IS.toList <$> stateTVar tableState.dirties \dirties -> (dirties, mempty)
                        mDirties <- forM dirties \dirty -> do
                            NM.lookup tableState.objects (unsafeFrom dirty) >>= \case
                                Just obj -> do
                                    (x, y) <- readTVar obj.coord
                                    pure $ Just (obj.elt, x, y)
                                Nothing -> pure Nothing
                        case catMaybes mDirties of
                            [] -> pure Nothing
                            xs -> pure $ Just (gameID, name, xs)
                    Nothing -> pure Nothing
                Selecting -> pure Nothing

    let saveGame :: Text -> TableState -> ProcessIO ()
        saveGame name ts = do
            gid <-
                GameID
                    <$> dbInsert
                        db
                        "INSERT INTO games (name, game) VALUES (:name, :game)"
                        [":name" := name, ":game" := into @Text ts.current.kind]
            let newGame = Game ts.current.kind (Just (gid, name))
                addGame = #games %~ (newGame :)
                setStatus = #status .~ Playing (ts & #current .~ newGame)
            (objects, mDirtyObjs, appState) <- atomically do
                objs <- NM.elems ts.objects
                dirties <- collectDirties
                (objs,dirties,) <$> updateState (addGame . setStatus)

            sendsHtml ctx.shared.clients (renderLoader appState ctx.wid)
            forM_ mDirtyObjs \(gameID, _name, dirtyObjs) -> do
                logInfo "Saving board before loading" ["count" .= length dirtyObjs]
                forM_ dirtyObjs (replaceObjectDB db gameID)

            logInfo "Creating game" ["name" .= name, "count" .= length objects]
            -- save current state
            forM_ objects \obj -> do
                (x, y) <- readTVarIO obj.coord
                insertObjectDB db gid (obj.elt, x, y)

    spawnThread_ $ forever do
        sleep 5_000
        mDirtyObjs <- atomically collectDirties
        forM mDirtyObjs \(gameID, name, dirtyObjs) -> do
            logInfo "Background saving" ["count" .= length dirtyObjs]
            forM_ dirtyObjs (replaceObjectDB db gameID)
            sendsHtml ctx.shared.clients (renderCurrentGame ctx.wid Nothing name)

    forever do
        atomically (readPipe ctx.pipe) >>= \case
            AppDisplay (UserJoined client) -> do
                appState <- readTVarIO tAppState
                atomically $ sendHtml client $ mountUI appState ctx.wid
                refreshUI appState (atomically . sendBinary client)
            AppData ev -> case decodeJSON @TableEvent (from ev.buffer) of
                Just tableEvent ->
                    (.status) <$> readTVarIO tAppState >>= \case
                        Playing tableState -> clientHandler tableState ev.client tableEvent
                        _ -> logError "Received data event during selection" ["ev" .= ev]
                Nothing -> logError "Unknown event" ["ev" .= ev]
            AppTrigger ev -> case ev.trigger of
                "new-game" -> do
                    appState <- atomically $ updateState (#status .~ Selecting)
                    sendsHtml ctx.shared.clients (mountUI appState ctx.wid)
                "start-game" -> case tryFrom @Text <$> ev.body ^? key "kind" . _JSON of
                    (Just (Right kind)) -> do
                        ts <- atomically (newTableState (Game kind Nothing))
                        appState <- atomically $ updateState (#status .~ Playing ts)
                        sendsHtml ctx.shared.clients (mountUI appState ctx.wid)
                    _ -> logError "Unknown game" ["ev" .= ev]
                "save-game" -> case ev.body ^? key "name" . _JSON of
                    Just name -> do
                        appState <- readTVarIO tAppState
                        case appState.status of
                            Playing tableState -> saveGame name tableState
                            Selecting -> logError "Can't save while selecting mode" []
                    _ -> logError "Missing name" ["ev" .= ev]
                "load-game" -> case ev.body ^? key "id" . _JSON of
                    Just gameID -> do
                        prevState <- readTVarIO tAppState
                        case filter (gameMatch gameID) prevState.games of
                            (game : _) -> do
                                ts <- tableStateFromDB db game
                                appState <- atomically $ updateState (#status .~ Playing ts)
                                sendsHtml ctx.shared.clients (mountUI appState ctx.wid)
                                refreshUI appState (sendsBinary ctx.shared.clients)
                            [] -> logError "Unknown game" ["ev" .= ev]
                    _ -> logError "Missing gameID" ["ev" .= ev]
                "del-game" -> case ev.body ^? key "id" . _JSON of
                    Just gameID -> do
                        let removeGame = #games %~ filter (not . gameMatch gameID)
                        appState <- atomically $ updateState removeGame
                        dbExecute db "DELETE FROM games WHERE id = :id" [":id" := gameID]
                        dbExecute db "DELETE FROM objects WHERE gameid = :id" [":id" := gameID]
                        sendsHtml ctx.shared.clients (renderLoader appState ctx.wid)
                    _ -> logError "Missing gameID" ["ev" .= ev]
                _ -> logError "Unknown trigger" ["ev" .= ev]
            _ -> pure ()

tabletopClient :: AppID -> GameKind -> Text
tabletopClient wid kind =
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

  const clickHandler = (isStatic, elt) => {
    if (glTT.current != null) {
      // drop the object
      if (glTT.trashed) {
        // the item is on the trash, tell the server
        sendJSONMessage(wid, {del: withoutWID(glTT.current.id)})
      }
      updateTrash(0, 0)
      glTT.current = null
    } else if (elt != null) {
      // Record object information for mousemouve handler
      glTT.boundingRect = elt.getBoundingClientRect()
      glTT.shiftX = event.clientX - glTT.boundingRect.left
      glTT.shiftY = event.clientY - glTT.boundingRect.top
      if (isStatic) {
        // request a new object
        const clickEv = {click: withoutWID(elt.id)}
        sendJSONMessage(wid, clickEv)
      } else {
        // grab it
        glTT.current = elt
      }
    }
  }

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
      const moveEv = {x, y, n: withoutWID(glTT.current.id)}
      updateTrash(event.pageX, event.pageY)
      sendJSONMessage(wid, moveEv)
      setPosition(glTT.current, x, y)
    }
  })

  winElt.onclick = () => {
      clickHandler(false, null)
  }

  // setupObject enables moving an element
  const setupObject = (elt, isStatic) => {
    elt.onclick = (ev) => {
      ev.stopPropagation()
      clickHandler(isStatic, elt)
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
      const srcPos = srcElt.getBoundingClientRect()
      setPosition(elt, srcPos.left, srcPos.top - srcPos.y / 2)
      winElt.appendChild(elt)

      if (body.grab) {
        glTT.current = elt
      }

      // Attach handlers for the source object
      setupObject(elt, false);
    } else if (body.elt) {
      const elt = document.getElementById(withWID(wid, body.elt))
      if (elt === null) {
        console.error("Invalid OID", body);
        return;
      }
      if (glTT.current && elt.id == glTT.current.id) {
        console.log("Object was stollen")
        glTT.current = null
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
    setupObject(document.getElementById(withWID(wid, id)), true)
  )
}
  |]
        <> "\nstartTabletop("
        <> Text.intercalate ", " initTableArgs
        <> ");"
  where
    initTableArgs =
        [ showT wid
        , showT (gameObjectsNames kind)
        ]
