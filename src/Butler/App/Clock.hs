module Butler.App.Clock (clockApp) where

import Butler

import Data.Time (formatTime)
import Data.Time.Clock (UTCTime (..))
import Data.Time.Format (defaultTimeLocale)
import Data.Time.LocalTime (LocalTime, TimeZone)
import Data.Time.LocalTime qualified

data ClockState = ClockUTC | ClockGMT | ClockEDT deriving (Show)

-- | drop millisecond from UTCTime
dropMilliSec :: UTCTime -> UTCTime
dropMilliSec (UTCTime day sec) = UTCTime day (fromInteger $ truncate sec)

clockValueHtml :: Monad m => WinID -> TimeZone -> LocalTime -> HtmlT m ()
clockValueHtml wid tz now =
    with div_ [id_ (withWID wid "clock-value")] do
        div_ $ toHtml (formatTime defaultTimeLocale "%F" now)
        with div_ [class_ "flex justify-center"] $ toHtml (formatTime defaultTimeLocale "%R" now <> " " <> show tz)

clockHtml :: Monad m => WinID -> HtmlT m () -> HtmlT m ()
clockHtml wid inner =
    with div_ [id_ (withWID wid "w"), class_ "bg-slate-200 rounded-full w-32 h-32 flex m-auto"] do
        with div_ [class_ "m-auto flex flex-col items-center justify-center"] do
            inner
            with div_ [] do
                tzButton "UTC"
                tzButton "GMT"
                tzButton "EDT"
  where
    tzButton tz =
        with
            span_
            [ id_ (withWID wid "clock-tz")
            , wsSend
            , class_ "cursor-pointer pl-1"
            , hxTrigger_ "click"
            , hxVals_ ("{\"v\": \"" <> tz <> "\"}")
            ]
            (toHtml tz)

clockContent :: WinID -> TVar ClockState -> TVar UTCTime -> HtmlT STM ()
clockContent wid s tNow = do
    state <- lift (readTVar s)
    now <- lift (readTVar tNow)
    let tz = case state of
            ClockUTC -> Data.Time.LocalTime.utc
            ClockGMT -> read "GMT"
            ClockEDT -> read "EDT"
    clockValueHtml wid tz (Data.Time.LocalTime.utcToLocalTime tz now)

clockApp :: App
clockApp =
    (defaultApp "clock" startClockApp)
        { tags = fromList ["Utility"]
        , description = "Display time"
        , size = Just (164, 164)
        , start = startClockApp
        }

startClockApp :: AppContext -> ProcessIO ()
startClockApp ctx = do
    state <- newTVarIO ClockUTC
    tNow <- newTVarIO =<< loadTime
    let mountUI :: HtmlT STM ()
        mountUI = clockHtml ctx.wid (clockContent ctx.wid state tNow)
    forever do
        -- TODO: adjust wait time based until the next minute starting second
        res <- atomically =<< waitTransaction 60_000 (readPipe ctx.pipe)
        case res of
            WaitTimeout{} -> pure ()
            WaitCompleted ae@AppDisplay{} -> sendHtmlOnConnect mountUI ae
            WaitCompleted (AppTrigger ev) -> case ev.body ^? key "v" . _String of
                Just "UTC" -> atomically $ writeTVar state ClockUTC
                Just "GMT" -> atomically $ writeTVar state ClockGMT
                Just "EDT" -> atomically $ writeTVar state ClockEDT
                _ -> logError "Unknown event" ["ev" .= ev]
            WaitCompleted _ -> pure ()
        atomically . writeTVar tNow =<< loadTime
        sendsHtml ctx.shared.clients (clockContent ctx.wid state tNow)
  where
    loadTime = dropMilliSec <$> liftIO getCurrentTime
