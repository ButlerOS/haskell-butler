module Butler.App.Mumbler where

import Data.ByteString qualified as BS

import Butler
import Butler.Core.Logger
import Butler.Core.NatMap qualified as NM

import Butler.Display.Session
import Butler.Display.User
import Butler.Service.SoundBlaster

mumblerApp :: App
mumblerApp =
    (defaultApp "mumbler" startMumbler)
        { tags = fromList ["Communication", "Sound"]
        , description = "Voice chat"
        }

----
-- Individual user
----
data MumblerStatus = Muted | PushedToTalk Time | Continuous
    deriving (Generic, ToJSON)

mumblerStatusClass :: MumblerStatus -> Text
mumblerStatusClass = \case
    Muted -> "ri-emotion-normal-line"
    PushedToTalk _ -> "ri-emotion-line"
    Continuous -> "ri-emotion-laugh-line"

data MumblerState = MumblerState
    { client :: DisplayClient
    , status :: TVar MumblerStatus
    , frameCount :: TVar Natural
    , channel :: TVar (Maybe SoundChannel)
    }

newMumblerState :: DisplayClient -> STM MumblerState
newMumblerState client = MumblerState client <$> newTVar Muted <*> newTVar 0 <*> newTVar Nothing

mumblerStateHtml :: MumblerState -> HtmlT STM ()
mumblerStateHtml ms = do
    username <- lift (readTVar ms.client.session.username)
    userIcon username
    toHtml username
    ": "
    icon <- mumblerStatusClass <$> lift (readTVar ms.status)
    with i_ [class_ (icon <> " pr-3")] mempty
    toHtml . show =<< lift (readTVar ms.frameCount)

----
-- Update state, handling double click to toggle continuous state
----
data MumblerUpdate
    = NewStatus MumblerStatus
    | MaybeMuting
    | NoUpdate

updateMumblerState :: MumblerState -> Time -> Bool -> STM MumblerUpdate
updateMumblerState ms now pressed = do
    let setStatus s = do
            writeTVar ms.status s
            pure (NewStatus s)
    readTVar ms.status >>= \case
        Muted | pressed -> setStatus (PushedToTalk now)
        PushedToTalk prev
            | -- double clicked
              pressed && (now - prev < 500) ->
                setStatus Continuous
            | not pressed && (now - prev > 500) -> setStatus Muted
            | not pressed -> pure MaybeMuting
        Continuous | pressed -> setStatus Muted
        _ -> pure NoUpdate

----
-- Collection of users
----
newtype AppState = AppState (NM.NatMap MumblerState)

newAppState :: DisplayClients -> STM AppState
newAppState clients = do
    state <- AppState <$> NM.newNatMap
    traverse_ (addMumbler state) =<< getClients clients
    pure state

addMumbler :: AppState -> DisplayClient -> STM ()
addMumbler (AppState nm) client = do
    clientState <- newMumblerState client
    NM.insert nm (from client.process.pid) clientState

delMumbler :: AppState -> DisplayClient -> STM ()
delMumbler (AppState nm) client = NM.delete nm (from client.process.pid)

getMumbler :: AppState -> DisplayClient -> STM (Maybe MumblerState)
getMumbler (AppState nm) client = NM.lookup nm (from client.process.pid)

appStateHtml :: WinID -> AppState -> HtmlT STM ()
appStateHtml wid (AppState nm) = do
    with ul_ [id_ (withWID wid "mumblers"), class_ "list-disc list-inside"] do
        users <- lift (NM.elems nm)
        traverse_ (li_ . mumblerStateHtml) users

----
-- App implementation
----
startMumbler :: AppContext -> ProcessIO ()
startMumbler ctx = do
    let clients = ctx.shared.clients
        wid = ctx.wid
    soundCard <- getSoundCard ctx
    (state, soundEvents) <- atomically do
        (,) <$> newAppState soundCard.clients <*> newReaderChan soundCard.events

    let
        sendEv (ev :: Word8) = "butlerDataSocketSend(new Uint8Array([" <> showT wid <> ", " <> showT ev <> "]))"
        pressedColor = "bg-red-500 px-10"
        readyColor = "bg-blue-500 hover:bg-blue-700"
        btn color (title :: Text) =
            with
                button_
                [ wid_ wid "mumbler-btn"
                , class_ (color <> " text-white font-bold p-1 rounded")
                , onmousedown_ (sendEv 1)
                , onmouseup_ (sendEv 0)
                ]
                (toHtml title)
        msBtn = \case
            Muted -> btn readyColor "Push-to-talk"
            PushedToTalk _ -> btn pressedColor "Talk"
            Continuous -> btn pressedColor "Cont"

        ico color icon =
            with
                i_
                [ wid_ wid "mumbler-ico"
                , class_ (color <> " " <> icon)
                , onmousedown_ (sendEv 1)
                , onmouseup_ (sendEv 0)
                ]
                mempty
        msIco status =
            let color = case status of
                    Muted -> readyColor
                    _ -> "bg-red-500"
             in ico color (mumblerStatusClass status)

        drawDebug =
            with div_ [id_ "soundblaster-debug", class_ "pt-5"] do
                drawList soundCard.channels "Connections:" (soundChannelHtml soundCard)
                drawList soundCard.receivers "Receiving:" soundReceiverHtml
        mountUI = do
            with div_ [wid_ wid "w"] do
                btn readyColor "Push-to-talk"
                appStateHtml wid state
                drawDebug
            with div_ [wid_ wid "tray"] do
                ico readyColor (mumblerStatusClass Muted)

        updateUI dataClient status = do
            atomically $ sendHtml dataClient do
                msBtn status
                msIco status
            sendsHtml clients $ appStateHtml wid state

        tryUpdateMumbler ms dataClient = \case
            NoUpdate -> pure ()
            NewStatus s -> do
                updateUI dataClient s
                case s of
                    Muted -> do
                        atomically (stopSoundReceiver soundCard wid dataClient)
                        -- wait for the very last frame before pausing the channel
                        void $ spawnThread do
                            sleep 500
                            atomically do
                                (msStatus, mChannel) <- (,) <$> readTVar ms.status <*> readTVar ms.channel
                                case (msStatus, mChannel) of
                                    (Muted, Just sc) -> do
                                        -- User is still muted, pause the chan
                                        pauseChannel soundCard sc
                                    _ -> pure ()
                    PushedToTalk _ -> atomically do
                        startClientRecorder soundCard wid dataClient
                    _ -> pure ()
            MaybeMuting -> void $ spawnThread do
                -- wait for mouseup event
                sleep 500
                readTVarIO ms.status >>= \case
                    PushedToTalk _ -> do
                        -- user didn't double click for continuous mode
                        atomically do
                            writeTVar ms.status Muted
                            stopSoundReceiver soundCard wid dataClient
                        updateUI dataClient Muted
                    _ -> pure ()

        updateMumbler dataClient pressed =
            atomically (getMumbler state dataClient) >>= \case
                Nothing -> logError "no mclient?" ["client" .= dataClient]
                Just ms -> do
                    now <- getTime
                    atomically (updateMumblerState ms now pressed)
                        >>= tryUpdateMumbler ms dataClient

        handleAudioEvent ms buf mFrame = do
            mSoundChannel <- atomically do
                msStatus <- readTVar ms.status
                mChannel <- readTVar ms.channel
                channelClosed <- case mChannel of
                    Nothing -> pure True
                    Just c -> (== 0) <$> readTVar c.frameCount
                case (msStatus, mChannel) of
                    -- User is muted, and channel is already closed
                    (Muted, _) | channelClosed -> pure Nothing
                    -- User is definitely muted
                    (Muted, Nothing) -> pure Nothing
                    -- User is ready
                    (_, Just sc) -> pure (Just sc)
                    (_, Nothing) -> do
                        username <- readTVar ms.client.session.username
                        let name = SoundChannelName $ showT wid <> "-mumbler-" <> from username
                        let filterSelf c = c.process.pid /= ms.client.process.pid
                        sc <- startSoundChannelKeep filterSelf soundCard wid name
                        setSoundChannelClient sc SoundClientMuted ms.client
                        writeTVar ms.channel (Just sc)
                        pure (Just sc)
            forM_ mSoundChannel \soundChannel -> do
                atomically (modifyTVar' ms.frameCount (+ 1))
                atomically (feedChannel soundCard soundChannel buf mFrame)

        handleEvent = forever do
            ev <- atomically (Left <$> readPipe ctx.pipe <|> Right <$> readTChan soundEvents)
            case ev of
                Left (AppDisplay (UserJoined client)) -> atomically $ sendHtml client mountUI
                Left (AppData de) -> do
                    case BS.uncons de.buffer of
                        Just (0, "") -> updateMumbler de.client False
                        Just (1, "") -> updateMumbler de.client True
                        _ -> logError "unknown ev" ["ev" .= BSLog de.buffer]
                Right (SoundUserJoined client) -> do
                    atomically (addMumbler state client)
                    sendsHtml clients $ appStateHtml wid state
                Right (SoundUserLeft client) -> do
                    atomically do
                        getMumbler state client >>= \case
                            Nothing -> pure ()
                            Just ms ->
                                readTVar ms.channel >>= \case
                                    Nothing -> pure ()
                                    Just c -> stopSoundChannel soundCard c
                    atomically (delMumbler state client)
                    sendsHtml clients $ appStateHtml wid state
                Right (SoundReceiveEvent client buf mFrame) -> do
                    atomically (getMumbler state client) >>= \case
                        Nothing -> logError "no mclient?" ["client" .= client]
                        Just ms -> handleAudioEvent ms buf mFrame
                _ -> pure ()

            -- update statistic
            case ev of
                Right (SoundReceiveEvent{}) -> pure ()
                Right _ -> sendsHtml clients drawDebug
                _ -> pure ()

    handleEvent `finally` atomically do
        -- remove active sound channels
        let AppState nm = state
        users <- NM.elems nm
        forM_ users \user -> do
            readTVar user.channel >>= \case
                Nothing -> pure ()
                Just sc -> stopSoundChannel soundCard sc
