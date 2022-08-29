{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Butler.App.Seat (seatApp) where

import Butler.Prelude
import Data.Text qualified as Text

import Butler.Desktop
import Butler.GUI
import Butler.Logger
import Butler.NatMap qualified as NM
import Butler.Process
import Butler.Session

import Data.Hashable (Hashable (hash))

sessionColor :: SessionID -> Text
sessionColor session =
    let k = hash session
     in "hsl(" <> from (show $ k `mod` 300) <> ", 60%, 50%)"

renderSeat :: DesktopClient -> HtmlT STM ()
renderSeat dclient = do
    let color = "color : " <> sessionColor dclient.client.session
        Pid idx = dclient.client.process.pid
    with
        i_
        [ id_ ("seat-" <> showT idx)
        , class_ "ri-user-fill bg-stone-600 p-0.5"
        , style_ color
        , title_ "username"
        ]
        mempty
    with
        span_
        [ id_ ("cursor-" <> showT idx)
        , class_ "pointer-events-none rounded-xl w-2 h-2 font-bold fixed z-50 "
        , style_ color
        ]
        "°"
    with div_ [id_ "seat-script"] mempty

seatApp :: Desktop -> ProcessIO GuiApp
seatApp desktop = do
    let clientHandler chan dclient buf =
            case (buf ^? key "x" . _Integer, buf ^? key "y" . _Integer) of
                (Just (unsafeFrom -> x), Just (unsafeFrom -> y)) -> do
                    -- logInfo "Got seat pos" ["x" .= x, "y" .= y]
                    atomically $ writeTVar dclient.cursor (Just (x, y))
                    broadcastDesktopMessage desktop (const True) chan buf
                _ -> case buf ^? key "ev" . _String of
                    Just "connected" -> do
                        let Pid idx = dclient.client.process.pid
                        broadcastMessageT desktop do
                            with div_ [id_ "seats", hxSwapOob_ "afterbegin"] do
                                renderSeat dclient
                        sendMessage dclient.client (dataMessage chan $ "{\"client\": " <> from (show idx) <> "}")
                    _ -> logError "invalid seat ev" ["buf" .= BSLog buf]

    chan <- atomically (newHandler desktop clientHandler)

    let removeSeat :: DesktopClient -> ProcessIO ()
        removeSeat dclient = do
            let Pid idx = dclient.client.process.pid
            broadcastMessageT desktop do
                with div_ [id_ $ "cursor-" <> showT idx, hxSwapOob_ "delete"] mempty
                with div_ [id_ $ "seat-" <> showT idx, hxSwapOob_ "delete"] mempty

        {-
                broadcastSeats :: (Natural, DesktopClient) -> (Int, Int) -> ProcessIO ()
                broadcastSeats (_idx, fromSeat) pos = do
                    atomically $ writeTVar fromSeat.cursor (Just pos)
                    currentSeats <- atomically $ NM.elemsIndex seats.seats
                    forM_ currentSeats $ \(idx, seat) -> do
                        let body = object ["seat" .= idx, "pos" .= pos]
                        v <- try (clientMessage seat.client (from $ encodeJSON body))
                        case v of
                            Left (_ :: SomeException) -> do
                                logError "seat down" ["seat" .= seat]
                                atomically $ removeSeat seats (idx, seat)
                            Right () -> pure ()
        -}
        cursorToggle session status = do
            let color = "color : " <> sessionColor session
                bg = if status then "bg-stone-400" else "bg-stone-800"
            with
                i_
                [ style_ color
                , class_ $ "ri-cursor-fill " <> bg <> " rounded-xl px-0.5 text-bold text-xl"
                , title_ (if status then "Hide cursor" else "Show cursor")
                , wsSend
                , hxTrigger_ "click"
                , id_ "toggle-cursor"
                , encodeVal ["running" .= status]
                ]
                do
                    script_ $ "seatCursor = " <> if status then "true" else "false" <> ";"

        tray :: DisplayClient -> HtmlT STM ()
        tray client = do
            with div_ [id_ "seats", class_ "flex content-center mr-1"] do
                currentSeats <- lift (NM.elems desktop.clients)
                traverse_ renderSeat currentSeats
                cursorToggle client.session True
                with
                    i_
                    [ class_ "ri-keyboard-box-fill bg-stone-400 rounded-xl px-0.5 text-bold text-xl"
                    , hyper_ $
                        Text.unlines
                            [ "on click"
                            , popup "Not Implemented"
                            ]
                    ]
                    mempty
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

                script_ (seatClient chan)

    newGuiApp2 "seat" Nothing (pure . tray) emptyDraw emptyDraw ["toggle-cursor"] \app -> do
        forever do
            ev <- atomically $ readPipe app.events
            case ev.body ^? key "running" . _Bool of
                Just status -> do
                    logInfo "got ev" ["running" .= status]
                {-
                seatM <- atomically (getSeat seats ev.client.session)
                case seatM of
                    Just seat -> do
                        clientMessageT ev.client do
                            cursorToggle ev.client.session (not status)

                        when status do
                            broadcastSeats seat (-100, -100)
                    Nothing -> logError "Unknown seat" []
                -}
                Nothing -> logError "Invalid ev" ["ev" .= ev]

seatClient :: Natural -> Text
seatClient chan =
    [raw|
globalThis.startCursorClient = (chan, idx) => {
}
function seatClient(chan) {
  butlerDataHandlers[chan] = buf => {
    let body = decodeJSON(buf)
    // console.log("Got seat ev", body)
    if (body.client) {
      window.onmousemove = debounceData(100, ev => {
        // console.log("Got mouse ev", ev)
        return encodeDataMessage(chan, {x: ev.clientX, y: ev.clientY, seat: body.client})
      })
    } else {
      let div = htmx.find("#cursor-" + body.seat);
      div.style.left = (body.x - 6) + "px";
      div.style.top = (body.y - 15) + "px";
    }
  }
  // wait for data conn to be ready
  setTimeout(() => {
    butlerDataSocket.send(encodeDataMessage(chan, {ev: "connected"}))
  }, 1000)
}
|]
        <> "seatClient("
        <> showT chan
        <> ")"
