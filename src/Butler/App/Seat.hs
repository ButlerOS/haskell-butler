module Butler.App.Seat (seatApp) where

import Butler.Prelude
import Data.Aeson
import Data.Map.Strict qualified as Map

import Butler.Desktop
import Butler.Display
import Butler.Frame
import Butler.GUI
import Butler.Logger
import Butler.Process
import Butler.Session
import Butler.SoundBlaster
import Butler.User

newtype Seats = Seats (TVar (Map Endpoint Seat))

newSeats :: STM Seats
newSeats = Seats <$> newTVar mempty

getSeats :: Seats -> STM [Seat]
getSeats (Seats seats) = Map.elems <$> readTVar seats

getSeat :: Seats -> DisplayClient -> STM (Maybe Seat)
getSeat (Seats seats) client = Map.lookup client.endpoint <$> readTVar seats

addSeat :: Seats -> Seat -> STM ()
addSeat (Seats seats) seat = modifyTVar' seats (Map.insert seat.client.endpoint seat)

delSeat :: Seats -> DisplayClient -> STM (Maybe Seat)
delSeat (Seats xs) client = do
    old <- getSeat (Seats xs) client
    modifyTVar' xs (Map.delete client.endpoint)
    pure old

data Seat = Seat
    { client :: DisplayClient
    , resolution :: TVar (Int, Int)
    , cursor :: TVar (Int, Int)
    }

newSeat :: DisplayClient -> (Int, Int) -> STM Seat
newSeat client resolution = Seat client <$> newTVar resolution <*> newTVar (0, 0)

renderSeat :: Seat -> HtmlT STM ()
renderSeat seat = do
    let user = seat.client.session.username
        Pid idx = seat.client.process.pid
    with span_ [id_ ("seat-" <> showT idx)] $ userTabIcon user seat.client.tabID
    createSeatCursor user idx

createSeatCursor :: UserName -> Natural -> HtmlT STM ()
createSeatCursor user idx =
    with
        span_
        [ id_ ("cursor-" <> showT idx)
        , class_ "pointer-events-none rounded-xl w-2 h-2 font-bold fixed z-50"
        , userColorStyle user
        ]
        "°"

renderToggle :: Text -> [Attribute] -> Bool -> Text -> Text -> HtmlT STM ()
renderToggle icon attrs enabled onScript offScript = do
    let bg = if enabled then "bg-stone-400" else "bg-stone-800"
    with
        i_
        ( [ class_ $ icon <> " rounded-xl px-0.5 text-bold text-xl cursor-pointer " <> bg
          , wsSend
          , hxTrigger_ "click"
          , encodeVal ["running" .= enabled]
          ]
            <> attrs
        )
        do
            script_ $ if enabled then onScript else offScript

renderCursorToggle :: UserName -> Bool -> HtmlT STM ()
renderCursorToggle username enabled = do
    renderToggle
        "ri-cursor-fill"
        [ userColorStyle username
        , title_ (if enabled then "Hide cursor" else "Show cursor")
        , id_ "toggle-cursor"
        ]
        enabled
        "window.addEventListener('mousemove', mouseHandler)"
        "window.removeEventListener('mousemove', mouseHandler)"

renderAudioToggle :: UserName -> Bool -> HtmlT STM ()
renderAudioToggle username enabled = do
    renderToggle
        "ri-volume-up-line"
        [ userColorStyle username
        , title_ (if enabled then "Mute" else "Listen")
        , id_ "toggle-audio"
        ]
        enabled
        "startAudio()"
        "stopAudio()"

appendSeat :: Seat -> HtmlT STM ()
appendSeat seat =
    with div_ [id_ "current-seats", hxSwapOob_ "afterbegin"] do
        renderSeat seat

renderSeatTray :: Session -> ChannelID -> ChannelID -> Seats -> HtmlT STM ()
renderSeatTray session chan audioChan seats = do
    with div_ [id_ "seats", class_ "flex content-center mr-1"] do
        script_ $
            mconcat
                [ seatClient chan
                , soundClient audioChan
                ]
        with div_ [id_ "current-seats"] do
            traverse_ renderSeat =<< lift (getSeats seats)
        renderCursorToggle session.username True
        renderAudioToggle session.username True

data SeatEvent
    = SeatEventResolution Int Int
    | SeatEventPosition Int Int
    deriving (Generic, ToJSON)

instance FromJSON SeatEvent where
    parseJSON = withObject "SeatEvent" $ \obj -> do
        mX <- obj .:? "x"
        mW <- obj .:? "w"
        case (mX, mW) of
            (Just x, Nothing) -> SeatEventPosition x <$> obj .: "y"
            (Nothing, Just w) -> SeatEventResolution w <$> obj .: "h"
            _ -> fail "x or w attribute missing"

seatApp :: Desktop -> ProcessIO GuiApp
seatApp desktop = do
    seats <- atomically newSeats

    let clientHandler _ chan client buf = case eitherDecode' (from buf) of
            Right ev -> do
                (isNew, seat, action) <- atomically do
                    mSeat <- getSeat seats client
                    -- create new seat if needed
                    seat <- case mSeat of
                        Nothing -> do
                            seat <- newSeat client (42, 42)
                            addSeat seats seat
                            pure seat
                        Just seat -> pure seat
                    -- handle seat event
                    (isNothing mSeat,seat,) <$> case ev of
                        SeatEventResolution w h -> do
                            writeTVar seat.resolution (w, h)
                            pure do
                                logInfo "seat resolution" ["client" .= client, "size" .= (w, h)]
                        SeatEventPosition x y -> do
                            writeTVar seat.cursor (x, y)
                            pure do
                                let cbuf = encodeJSON (object ["x" .= x, "y" .= y, "pid" .= client.process.pid])
                                broadcastDesktopMessage desktop (const True) chan (from cbuf)
                when isNew do
                    clientsBroadcast desktop.hclients (appendSeat seat)
                action
            Left err -> logError "invalid json" ["ev" .= BSLog buf, "err" .= err]

    chan <- atomically (newHandler desktop.handlers clientHandler)
    desktopEvents <- atomically (newReaderChan desktop.desktopEvents)

    let removeSeat :: Monad m => DisplayClient -> HtmlT m ()
        removeSeat client = do
            let Pid idx = client.process.pid
            with div_ [id_ $ "cursor-" <> showT idx, hxSwapOob_ "delete"] mempty
            with div_ [id_ $ "seat-" <> showT idx, hxSwapOob_ "delete"] mempty

    let tray :: DisplayClient -> HtmlT STM ()
        tray client = renderSeatTray client.session chan audioChannel seats

    newGuiApp2 "seat" Nothing (pure . tray) emptyDraw emptyDraw ["toggle-cursor", "toggle-audio"] \app -> do
        spawnThread_ $ forever do
            ev <- atomically (readTChan desktopEvents)
            -- logInfo "got desktop ev" ["ev" .= ev]
            case ev of
                UserDisconnected "data" client -> do
                    seatM <- atomically do
                        delSeat seats client
                    case seatM of
                        Just seat -> broadcastHtmlT desktop $ removeSeat seat.client
                        Nothing -> pure ()
                _ -> pure ()

        forever do
            ev <- atomically $ readPipe app.events
            case ev.trigger of
                "toggle-cursor" ->
                    let running = fromMaybe True (ev.body ^? key "running" . _Bool)
                        btn = renderCursorToggle ev.client.session.username (not running)
                     in safeSend desktop.hclients sendHtml ev.client btn
                "toggle-audio" ->
                    let running = fromMaybe True (ev.body ^? key "running" . _Bool)
                        btn = renderAudioToggle ev.client.session.username (not running)
                     in safeSend desktop.hclients sendHtml ev.client btn
                _ -> logError "Invalid ev" ["ev" .= ev]

seatClient :: ChannelID -> Text
seatClient chan =
    [raw|
function seatClient(chan) {
  butlerDataHandlers[chan] = buf => {
    let body = decodeJSON(buf)
    // console.log("Got seat ev", body)
    let div = htmx.find("#cursor-" + body.pid);
    div.style.left = (body.x - 6) + "px";
    div.style.top = (body.y - 15) + "px";
  }
  globalThis.mouseHandler = debounceData(100, ev => {
        // console.log("Got mouse ev", ev)
        return encodeDataMessage(chan, {x: ev.clientX, y: ev.clientY})
    });
  butlerDataSocketSend(encodeDataMessage(chan, {w: window.innerWidth, h: window.innerHeight}));
}
|]
        <> "seatClient("
        <> showT chan
        <> ")"
