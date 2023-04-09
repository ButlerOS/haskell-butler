module Butler.App.Painter (painterApp) where

import Butler
import Butler.Frame

painterApp :: App
painterApp =
    (defaultApp "painter" startPainterApp)
        { tags = fromList ["Utility", "Graphic"]
        , description = "Paint!"
        , start = startPainterApp
        }

data Coord = Coord {x :: Word, y :: Word}
    deriving (Eq)

instance ToJSON Coord where
    toJSON c = toJSON [c.x, c.y]

data Color = Black | White | Red | Blue
    deriving (Eq, Enum, Bounded, Show, Read)
    deriving (Generic, FromJSON, ToJSON)

colorStyle :: Color -> Text
colorStyle = \case
    Black -> "rgb(0, 0, 0)"
    White -> "rgb(255, 255, 255)"
    Red -> "rgb(200, 23, 23)"
    Blue -> "rgb(23, 23, 200)"

data Shape = Rectangle Color Coord Coord
    deriving (Eq)

instance ToJSON Shape where
    toJSON = \case
        Rectangle color coord size -> object ["type" .= ("rectangle" :: Text), "fill" .= colorStyle color, "coord" .= coord, "size" .= size]

newtype Image = Image [Shape]

newImage :: Image
newImage = Image []

data Tool = MkRectangle

data Painter = Painter
    { tool :: Tool
    , lastClick :: Maybe Coord
    , fill :: Color
    , history :: [Shape]
    }
    deriving (Generic)

newPainter :: Painter
newPainter = Painter MkRectangle Nothing Black []

startPainterApp :: AppContext -> ProcessIO ()
startPainterApp ctx = do
    tImage <- newTVarIO newImage
    painters <- atomically newClientsData

    let
        mountControllerUI :: TVar Painter -> HtmlT STM ()
        mountControllerUI tPainter = with div_ [wid_ ctx.wid "ctrl"] do
            painter <- lift (readTVar tPainter)
            let selCls = "rounded border-4 border-red-200"
            with div_ [class_ "flex flex-row"] do
                "Tool: "
                with button_ [class_ "mx-1"] "Clear"
                with button_ [class_ $ "mx-1 " <> selCls] "Rectangle"
            with div_ [class_ "flex flex-row"] do
                "Fill: "
                forM_ [minBound .. maxBound] \color -> do
                    let clr = style_ $ "background-color: " <> colorStyle color
                        selected
                            | color /= painter.fill = ""
                            | otherwise = selCls
                    withTrigger "click" ctx.wid "color" ["color" .= color] button_ [clr, class_ $ "mx-1 " <> selected] (toHtml $ show color)

        mountUI client = do
            with div_ [wid_ ctx.wid "w"] do
                with canvas_ [wid_ ctx.wid "img", class_ "border", width_ "300", height_ "300"] mempty
                script_ (painterClient ctx.wid)
                mClientData <- lift (lookupClientData painters client)
                forM_ mClientData mountControllerUI

        removeLastClick :: Painter -> Painter
        removeLastClick = #lastClick .~ Nothing

        handleClick :: _ -> Coord -> ProcessIO ()
        handleClick tPainter coord = do
            -- Check if a new shape is completed
            mShape <- atomically $ stateTVar tPainter \painter -> case painter.lastClick of
                Just p1@(Coord px py) ->
                    let newShape = Rectangle painter.fill p1 (Coord (coord.x - px) (coord.y - py))
                     in (Just newShape, painter & (#history %~ (newShape :)) . removeLastClick)
                Nothing -> (Nothing, painter & #lastClick ?~ coord)
            forM_ mShape \shape -> do
                atomically $ modifyTVar' tImage \(Image shapes) -> Image (shape : shapes)
                sendsBinary ctx.shared.clients $ encodeMessage (from ctx.wid) $ encodeJSON shape

    forever do
        atomically (readPipe ctx.pipe) >>= \case
            AppDisplay (UserJoined client) -> do
                atomically $ addClientsData painters client newPainter
                atomically $ sendHtml client (mountUI client)
                -- Draw the current image
                Image shapes <- readTVarIO tImage
                forM_ (reverse shapes) \shape -> do
                    atomically $ sendBinary client (encodeMessage (from ctx.wid) (encodeJSON shape))
            AppDisplay (UserLeft client) -> atomically $ delClientsData painters client
            AppTrigger ev -> withClientsData painters ev.client \tPainter -> case ev.trigger of
                "color" -> case ev.body ^? key "color" . _JSON of
                    Just color -> do
                        -- Change the painter fill color
                        atomically $ modifyTVar' tPainter $ (#fill .~ color) . removeLastClick
                        atomically $ sendHtml ev.client (mountControllerUI tPainter)
                    Nothing -> logError "Misisng color" ["ev" .= ev]
                _ -> logError "Unknown ev" ["ev" .= ev]
            AppData ev -> withClientsData painters ev.client \tPainter -> case decodeJSON (from ev.buffer) of
                Just (x, y) -> handleClick tPainter (Coord x y)
                Nothing -> logError "Unknown ev" ["ev" .= ev]
            _ -> pure ()

painterClient :: AppID -> Text
painterClient wid =
    [raw|
function setupPainterClient(wid) {
  const elt = document.getElementById(withWID(wid, "img"))
  const ctx = elt.getContext("2d")

  elt.onmousedown = (event) => {
    // Ignore non-left click event
    if (event.buttons != 1) {
      return false;
    }
    // Record object information for mousemouve handler
    const boundingRect = elt.getBoundingClientRect()
    const x = event.clientX - boundingRect.left
    const y = event.clientY - boundingRect.top
    butlerDataSocketSend(encodeDataMessage(wid, [x, y]));
  }

  butlerDataHandlers[wid] = buf => {
    const body = decodeJSON(buf)
    if (body.type == "rectangle") {
      ctx.fillStyle = body.fill
      ctx.fillRect(body.coord[0], body.coord[1], body.size[0], body.size[1])
    } else {
      console.error("Unknown ev", body)
    }
  }
}
|]
        <> ("\nsetupPainterClient(" <> showT wid <> ");")
