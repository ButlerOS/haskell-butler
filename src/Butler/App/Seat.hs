module Butler.App.Seat (seatApp) where

import Butler.Prelude
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text

import Butler.Desktop
import Butler.Display
import Butler.Frame
import Butler.GUI
import Butler.Logger
import Butler.Process
import Butler.Session
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
    , _resolution :: TVar (Int, Int)
    , cursor :: TVar (Int, Int)
    }

newSeat :: DisplayClient -> (Int, Int) -> STM Seat
newSeat client resolution = Seat client <$> newTVar resolution <*> newTVar (0, 0)

renderSeat :: Seat -> HtmlT STM ()
renderSeat seat = do
    let user = seat.client.session.username
        Pid idx = seat.client.process.pid
    with span_ [id_ ("seat-" <> showT idx)] $ userIcon user
    with
        span_
        [ id_ ("cursor-" <> showT idx)
        , class_ "pointer-events-none rounded-xl w-2 h-2 font-bold fixed z-50 "
        , userColorStyle user
        ]
        "°"
    with div_ [id_ "seat-script"] mempty

renderCursorToggle :: UserName -> Maybe Bool -> HtmlT STM ()
renderCursorToggle username statusM = do
    let bg = if fromMaybe True statusM then "bg-stone-400" else "bg-stone-800"
    with
        i_
        [ userColorStyle username
        , class_ $ "ri-cursor-fill " <> bg <> " rounded-xl px-0.5 text-bold text-xl"
        , title_ (if fromMaybe True statusM then "Hide cursor" else "Show cursor")
        , wsSend
        , hxTrigger_ "click"
        , id_ "toggle-cursor"
        , encodeVal ["running" .= statusM]
        ]
        do
            case statusM of
                Just status
                    | status -> script_ "window.onmousemove = mouseHandler"
                    | otherwise -> script_ "window.onmousemove = null"
                Nothing -> mempty

appendSeat :: Seat -> HtmlT STM ()
appendSeat seat =
    with div_ [id_ "seats", hxSwapOob_ "afterbegin"] do
        renderSeat seat

renderSeats :: UserName -> Seats -> ChannelID -> HtmlT STM ()
renderSeats username seats chan = do
    xs <- lift (getSeats seats)
    with div_ [id_ "seats", class_ "flex content-center mr-1"] do
        traverse_ renderSeat xs
        renderCursorToggle username Nothing
        -- class_ "ri-keyboard-box-fill bg-stone-400 rounded-xl px-0.5 text-bold text-xl"
        with
            i_
            [ class_ "ri-volume-up-line bg-stone-800 rounded-xl px-0.5 text-bold text-xl"
            , hyper_ $
                Text.unlines
                    [ "on click"
                    , popup "Not Implemented"
                    ]
            ]
            mempty

        script_ (seatClient (from chan))

seatApp :: Desktop -> ProcessIO GuiApp
seatApp desktop = do
    seats <- atomically newSeats

    let clientHandler _ chan client buf =
            case (buf ^? key "w" . _Integer, buf ^? key "h" . _Integer) of
                (Just (unsafeFrom -> w), Just (unsafeFrom -> h)) -> do
                    seat <- atomically do
                        seat <- newSeat client (w, h)
                        addSeat seats seat
                        pure seat
                    logInfo "new seat" ["client" .= seat.client]
                    clientsBroadcast desktop.hclients (appendSeat seat)
                _ -> case (buf ^? key "x" . _Integer, buf ^? key "y" . _Integer) of
                    (Just (unsafeFrom -> x), Just (unsafeFrom -> y)) -> do
                        seatM <- atomically (getSeat seats client)
                        case seatM of
                            Just seat -> do
                                -- logInfo "Got seat pos" ["x" .= x, "y" .= y]
                                atomically $ writeTVar seat.cursor (x, y)
                                let cbuf = encodeJSON (object ["x" .= x, "y" .= y, "pid" .= client.process.pid])
                                broadcastDesktopMessage desktop (const True) chan (from cbuf)
                            Nothing -> logError "no seat" ["client" .= client]
                    _ ->
                        logError "unknown ev" ["buf" .= BSLog buf]

    chan <- atomically (newHandler desktop clientHandler)
    desktopEvents <- atomically (newReaderChan desktop.desktopEvents)

    let removeSeat :: Monad m => DisplayClient -> HtmlT m ()
        removeSeat client = do
            let Pid idx = client.process.pid
            with div_ [id_ $ "cursor-" <> showT idx, hxSwapOob_ "delete"] mempty
            with div_ [id_ $ "seat-" <> showT idx, hxSwapOob_ "delete"] mempty

    let tray :: DisplayClient -> HtmlT STM ()
        tray client = renderSeats client.session.username seats chan

    newGuiApp2 "seat" Nothing (pure . tray) emptyDraw emptyDraw ["toggle-cursor"] \app -> do
        spawnThread_ $ forever do
            ev <- atomically (readTChan desktopEvents)
            -- logInfo "got desktop ev" ["ev" .= ev]
            case ev of
                UserDisconnected "data" client -> do
                    seatM <- atomically $ delSeat seats client
                    case seatM of
                        Just seat -> broadcastMessageT desktop $ removeSeat seat.client
                        Nothing -> pure ()
                _ -> pure ()

        forever do
            ev <- atomically $ readPipe app.events
            case ev.trigger of
                "toggle-cursor" ->
                    let running = fromMaybe True (ev.body ^? key "running" . _Bool)
                        btn = renderCursorToggle ev.client.session.username (Just $ not running)
                     in safeSend desktop.hclients sendHtml ev.client btn
                _ -> logError "Invalid ev" ["ev" .= ev]

seatClient :: Natural -> Text
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
  // wait for data conn to be ready
  setTimeout(() => {
    butlerDataSocket.send(encodeDataMessage(chan, {w: window.innerWidth, h: window.innerHeight}))
    window.onmousemove = mouseHandler
  }, 1000)
}
|]
        <> "seatClient("
        <> showT chan
        <> ")"
