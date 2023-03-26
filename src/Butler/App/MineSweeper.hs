module Butler.App.MineSweeper (mineSweeperApp, startMineSweeper) where

import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as Aeson
import Data.Aeson.Text qualified as Aeson
import Data.Map qualified as Map
import Data.Text qualified as T
import Data.Text.Lazy (toStrict)
import System.Random (randomRIO)

import Butler

data MSState = MSState
    { board :: MSBoard
    , state :: MSGameState
    }
    deriving (Show)

data MSGameState = Play | Gameover deriving (Show)

data MSCellContent
    = Mine
    | Blank Int
    deriving (Show)

data MSCellStatus = Open | Hidden deriving (Show)

data MSCell = MSCell
    { cellContent :: MSCellContent
    , cellStatus :: MSCellStatus
    }
    deriving (Show)

data MSCellCoord = MSCellCoord
    { cx :: Int
    , cy :: Int
    }
    deriving (Show, Eq, Ord, Generic)

type MSBoard = Map.Map MSCellCoord MSCell

data MSEvent
    = NewGame MSBoard
    | OpenCell MSCellCoord
    deriving (Show)

initBoard :: IO MSBoard
initBoard = do
    let cellsCoords = [MSCellCoord x y | x <- [0 .. 9], y <- [0 .. 9]]
        blankBoard = Map.fromList $ map (,MSCell (Blank 0) Hidden) cellsCoords
    minesCoords <- getMinesCoords cellsCoords []
    pure $ setBoard blankBoard minesCoords
  where
    getMinesCoords :: [MSCellCoord] -> [MSCellCoord] -> IO [MSCellCoord]
    getMinesCoords availableCellsCords minesCoords =
        if length minesCoords == 9
            then pure minesCoords
            else do
                selectedIndex <- randomRIO (0, length availableCellsCords - 1)
                let selectedCoord = availableCellsCords !! selectedIndex
                    remainingCellsCords = filter (/= selectedCoord) availableCellsCords
                getMinesCoords remainingCellsCords (minesCoords <> [selectedCoord])
    setBoard :: MSBoard -> [MSCellCoord] -> MSBoard
    setBoard board minesCoords =
        let adjCellds = concatMap getAdjCellCoords minesCoords
            board' = installAdjCells board adjCellds
         in installMines board' minesCoords
      where
        installMines :: MSBoard -> [MSCellCoord] -> MSBoard
        installMines b cs = case cs of
            [] -> b
            [x] -> Map.insert x (MSCell Mine Hidden) b
            (x : xs) -> installMines (Map.insert x (MSCell Mine Hidden) b) xs
        installAdjCells :: MSBoard -> [MSCellCoord] -> MSBoard
        installAdjCells b cs = case cs of
            [] -> b
            [x] -> installAdjCell b x
            (x : xs) -> installAdjCells (installAdjCell b x) xs
        installAdjCell :: MSBoard -> MSCellCoord -> MSBoard
        installAdjCell b c =
            Map.insertWith
                ( \_ oldv ->
                    case oldv of
                        MSCell (Blank v) s -> MSCell (Blank (v + 1)) s
                        other -> other
                )
                c
                (MSCell (Blank 1) Hidden)
                b

getAdjCellCoords :: MSCellCoord -> [MSCellCoord]
getAdjCellCoords MSCellCoord{..} =
    let isInBoard (MSCellCoord cx' cy') = cx' >= 0 && cx' <= 9 && cy' >= 0 && cy' <= 9
     in filter
            isInBoard
            [ MSCellCoord (cx - 1) (cy - 1)
            , MSCellCoord (cx - 1) cy
            , MSCellCoord (cx - 1) (cy + 1)
            , MSCellCoord (cx + 1) (cy - 1)
            , MSCellCoord (cx + 1) cy
            , MSCellCoord (cx + 1) (cy + 1)
            , MSCellCoord cx (cy - 1)
            , MSCellCoord cx (cy + 1)
            ]

renderApp :: AppID -> TVar MSState -> HtmlT STM ()
renderApp wid state = do
    div_ [id_ (withWID wid "w"), class_ "w-60 border-2 border-gray-400 bg-gray-100"] $ do
        renderPanel wid state
        renderBoard wid state

renderPanel :: AppID -> TVar MSState -> HtmlT STM ()
renderPanel wid state = do
    appState <- lift (readTVar state)
    div_ [id_ (withWID wid "MSPanel"), class_ "bg-gray-200 m-1 flex justify-between"] $ do
        div_ [class_ "w-10"] "9 💣"
        withEvent (withWID wid "play") [mkHxVals [("play", "")]] $ div_ [class_ "bg-gray-300 border-2"] $ case appState.state of
            Play -> "🙂"
            Gameover -> "☹"
        div_ [class_ "w-10 text-right"] "0"

renderBoard :: AppID -> TVar MSState -> HtmlT STM ()
renderBoard wid state = do
    appState <- lift (readTVar state)
    div_ [id_ "MSBoard", class_ "grid grid-cols-10 gap-1"] $ do
        mapM_ (renderCell appState.state) $ Map.toList appState.board
  where
    renderCell :: MSGameState -> (MSCellCoord, MSCell) -> HtmlT STM ()
    renderCell gameState (cellCoords, cellState) =
        let cellId = mkHxVals [("cx", T.pack $ show $ cellCoords.cx), ("cy", T.pack $ show $ cellCoords.cy)]
         in installCellEvent gameState cellId $
                div_ [class_ "bg-gray-300 text-center"] $
                    case cellState of
                        MSCell (Blank v) Open
                            | v == 0 -> div_ [class_ "bg-gray-200 h-6 w-full "] ""
                            | v == 1 -> div_ [class_ "bg-gray-200 font-bold text-blue-700"] $ showCellValue v
                            | v == 2 -> div_ [class_ "bg-gray-200 font-bold text-green-700"] $ showCellValue v
                            | v == 3 -> div_ [class_ "bg-gray-200 font-bold text-red-700"] $ showCellValue v
                            | v == 4 -> div_ [class_ "bg-gray-200 font-bold text-blue-900"] $ showCellValue v
                            | v == 5 -> div_ [class_ "bg-gray-200 font-bold text-red-900"] $ showCellValue v
                            | v == 6 -> div_ [class_ "bg-gray-200 font-bold text-green-900"] $ showCellValue v
                            | v == 7 -> div_ [class_ "bg-gray-200 font-bold text-brown-700"] $ showCellValue v
                            | v == 8 -> div_ [class_ "bg-gray-200 font-bold text-black-700"] $ showCellValue v
                        MSCell (Blank _) Open -> error "Impossible case"
                        MSCell Mine Open -> div_ [class_ "bg-red-500"] "💣"
                        MSCell _ Hidden -> div_ [class_ "border-2 border-r-gray-400 border-b-gray-400 h-6 w-full"] ""
      where
        showCellValue :: Monad m => Int -> HtmlT m ()
        showCellValue = toHtml . show
        installCellEvent :: Monad m => MSGameState -> Attribute -> HtmlT m () -> HtmlT m ()
        installCellEvent gs cellId elm = case gs of
            Gameover -> elm
            Play -> withEvent (withWID wid "showCell") [cellId] elm

getCell :: MSCellCoord -> MSBoard -> Maybe MSCell
getCell = Map.lookup

openCell :: MSCellCoord -> MSBoard -> MSBoard
openCell = Map.update func
  where
    func :: MSCell -> Maybe MSCell
    func (MSCell content _) = Just $ MSCell content Open

isBlank0Cell :: MSCellCoord -> MSBoard -> Bool
isBlank0Cell cellCoord board = case getCell cellCoord board of
    Just (MSCell (Blank 0) _) -> True
    _ -> False

isHiddenCell :: MSCellCoord -> MSBoard -> Bool
isHiddenCell cellCoord board = case getCell cellCoord board of
    Just (MSCell _ Hidden) -> True
    _ -> False

isMineCell :: MSCellCoord -> MSBoard -> Bool
isMineCell cellCoord board = case getCell cellCoord board of
    Just (MSCell Mine _) -> True
    _ -> False

openAdjBlank0Cells :: MSCellCoord -> MSBoard -> MSBoard
openAdjBlank0Cells cellCoord board =
    if isBlank0Cell cellCoord board
        then openCells (getAdjCellCoords cellCoord) board
        else board
  where
    openCells :: [MSCellCoord] -> MSBoard -> MSBoard
    openCells cellsCoords b = case cellsCoords of
        [] -> b
        [x] -> openCell' x b
        (x : xs) -> openCells xs $ openCell' x b
    openCell' coord b =
        if isHiddenCell coord b
            then let nb = openCell coord b in openAdjBlank0Cells coord nb
            else b

withEvent :: Monad m => Text -> [Attribute] -> HtmlT m () -> HtmlT m ()
withEvent tId tAttrs elm = with elm ([id_ tId, wsSend' ""] <> tAttrs)
  where
    wsSend' = makeAttribute "ws-send"

mkHxVals :: [(Aeson.Key, Text)] -> Attribute
mkHxVals vals =
    hxVals
        . toStrict
        . Aeson.encodeToLazyText
        $ Aeson.fromList vals
  where
    hxVals = makeAttribute "hx-vals"

mineSweeperApp :: App
mineSweeperApp =
    (defaultApp "minesweeper" startMineSweeper)
        { tags = fromList ["Game"]
        , description = "game"
        , size = Just (240, 351)
        }

startMineSweeper :: AppContext -> ProcessIO ()
startMineSweeper ctx = do
    let clients = ctx.shared.clients
        wid = ctx.wid
    board <- liftIO initBoard
    state <- newTVarIO $ MSState board Play
    forever do
        res <- atomically =<< waitTransaction 60_000 (readPipe ctx.pipe)
        case res of
            WaitTimeout{} -> pure ()
            WaitCompleted (AppDisplay de) -> case de of
                UserJoined client -> atomically $ sendHtml client (renderApp wid state)
                _ -> pure ()
            WaitCompleted (AppTrigger ev) -> case ( ev.body ^? key "play" . _String
                                                  , ev.body ^? key "cx" . _String
                                                  , ev.body ^? key "cy" . _String
                                                  ) of
                (Just "", Nothing, Nothing) -> do
                    newBoard <- liftIO initBoard
                    atomically $ writeTVar state (MSState newBoard Play)
                (_, Just cxS, Just cyS) -> do
                    let cxM = readMaybe $ from cxS :: Maybe Int
                        cyM = readMaybe $ from cyS :: Maybe Int
                    case (cxM, cyM) of
                        (Just cx, Just cy) -> do
                            appState <- readTVarIO state
                            let cellCoord = MSCellCoord cx cy
                            if isMineCell cellCoord appState.board
                                then do
                                    atomically $
                                        writeTVar
                                            state
                                            (MSState (openCell cellCoord appState.board) Gameover)
                                else do
                                    let gs1 = openCell cellCoord appState.board
                                        gs2 = openAdjBlank0Cells cellCoord gs1
                                    atomically $ writeTVar state (MSState gs2 Play)
                        _ -> pure ()
                _ -> pure ()
            WaitCompleted _ -> pure ()
        sendsHtml clients $ renderApp wid state
