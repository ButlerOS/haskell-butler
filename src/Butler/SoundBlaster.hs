module Butler.SoundBlaster where

import Codec.EBML qualified as EBML
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types (Pair)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Lucid

import Butler.App
import Butler.Display
import Butler.Frame
import Butler.GUI
import Butler.Logger
import Butler.NatMap qualified as NM
import Butler.OS
import Butler.Pipe
import Butler.Prelude
import Butler.Session
import Butler.User

-------------------------------------------------------------------------------
-- Sound Card setup
-------------------------------------------------------------------------------
data SoundCard = SoundCard
    { wid :: WinID
    , clients :: DisplayClients
    , channels :: NM.NatMap SoundChannel
    , receivers :: NM.NatMap SoundReceiver
    , events :: BroadcastChan SoundCardEvent
    }

newSoundCard :: WinID -> STM SoundCard
newSoundCard wid = SoundCard wid <$> newDisplayClients <*> NM.newNatMap <*> NM.newNatMap <*> newBroadcastChan

data SoundCardEvent
    = SoundUserJoined DisplayClient
    | SoundUserLeft DisplayClient
    | SoundReceiverStarted
    | SoundReceiverStopped
    | SoundChannelEvent SoundChannelID
    | SoundReceiveEvent DisplayClient ByteString (Maybe EBML.StreamFrame)

addSoundClient :: SoundCard -> DisplayClient -> STM ()
addSoundClient sc client = do
    addClient sc.clients client

    -- attach to channels
    soundChannels <- NM.elems sc.channels
    traverse_ (\soundChannel -> startSoundChannelClient sc soundChannel False client) soundChannels

    broadcast sc.events (SoundUserJoined client)

delSoundClient :: SoundCard -> DisplayClient -> STM ()
delSoundClient sc client = do
    delClient sc.clients client

    -- remove from channels
    soundChannels <- NM.elems sc.channels
    traverse_ (flip delSoundChannelClient client) soundChannels

    broadcast sc.events (SoundUserLeft client)

-------------------------------------------------------------------------------
-- Sound Card HTML
-------------------------------------------------------------------------------
soundCardInfoHtml :: SoundCard -> HtmlT STM ()
soundCardInfoHtml sc = with div_ [id_ "sc-info"] do
    clients <- lift (getClients sc.clients)
    div_ $
        case clients of
            [] -> pure ()
            _ -> do
                with div_ [class_ "text-sm font-bold"] "Connected: "
                forM_ clients \client -> do
                    userTabIcon client.session.username client.tabID

    drawList sc.channels "Playing:" (soundChannelHtml sc)
    drawList sc.receivers "Receiving:" soundReceiverHtml

drawList :: NM.NatMap a -> HtmlT STM () -> (a -> HtmlT STM ()) -> HtmlT STM ()
drawList tv title render =
    div_ do
        lift (NM.elems tv) >>= \case
            [] -> pure ()
            elts -> do
                with div_ [class_ "text-sm font-bold"] title
                div_ do
                    with ul_ [class_ "list-disc list-inside"] do
                        forM_ elts \elt -> do
                            li_ (render elt)

soundChannelHtml :: SoundCard -> SoundChannel -> HtmlT STM ()
soundChannelHtml sc chan = do
    toHtml chan.name
    ": "
    clients <- lift (getClients sc.clients)
    forM_ clients \client -> do
        mClientStatus <- lift (getSoundChannelClient chan client)
        forM_ mClientStatus \clientStatus -> do
            with span_ [class_ "mr-2"] do
                userTabIcon client.session.username client.tabID
                clientStatusHtml clientStatus

soundReceiverHtml :: SoundReceiver -> HtmlT STM ()
soundReceiverHtml receiver = do
    userTabIcon receiver.client.session.username receiver.client.tabID
    ": "
    counter <- lift (readTVar receiver.counter)
    toHtml (show counter)

-------------------------------------------------------------------------------
-- Sound Channel (for player)
-------------------------------------------------------------------------------
data SoundChannel = SoundChannel
    { winID :: WinID
    , name :: SoundChannelName
    , id :: SoundChannelID
    , frameCount :: TVar Natural
    , statuses :: NM.NatMap SoundClientStatus
    }

newtype SoundChannelName = SoundChannelName Text
    deriving (Show, Generic)
    deriving newtype (Ord, Eq, Semigroup, Serialise, IsString, FromJSON, ToJSON, ToHtml)

newtype SoundChannelID = SoundChannelID Natural deriving newtype (Show, ToJSON)

instance From SoundChannelID Natural where
    from (SoundChannelID chan) = chan

instance From WinID SoundChannelID where
    from (WinID wid) = SoundChannelID (unsafeFrom wid)

instance From SoundChannelID WinID where
    from (SoundChannelID chan) = WinID (unsafeFrom chan)

data SoundClientStatus
    = SoundClientInitializing Bool
    | SoundClientPlaying
    | SoundClientMuted
    | SoundClientError Text

clientStatusHtml :: SoundClientStatus -> HtmlT STM ()
clientStatusHtml = \case
    SoundClientInitializing False -> tooltip "(w)" "waiting for data..."
    SoundClientInitializing True -> tooltip "(r)" "ready..."
    SoundClientPlaying -> ""
    SoundClientMuted -> tooltip "(m)" "muted"
    SoundClientError e -> "[E: " <> toHtml e <> "]"
  where
    tooltip n title = with span_ [title_ title] n

newSoundChannel :: WinID -> SoundChannelName -> Natural -> STM SoundChannel
newSoundChannel wid name k = SoundChannel wid name (SoundChannelID k) <$> newTVar 0 <*> NM.newNatMap

delSoundChannelClient :: SoundChannel -> DisplayClient -> STM ()
delSoundChannelClient soundChannel client = NM.delete soundChannel.statuses (from client.process.pid)

setSoundChannelClient :: SoundChannel -> SoundClientStatus -> DisplayClient -> STM ()
setSoundChannelClient soundChannel status client = NM.insert soundChannel.statuses (from client.process.pid) status

getSoundChannelClient :: SoundChannel -> DisplayClient -> STM (Maybe SoundClientStatus)
getSoundChannelClient soundChannel client = NM.lookup soundChannel.statuses (from client.process.pid)

startSoundChannelClient :: SoundCard -> SoundChannel -> Bool -> DisplayClient -> STM ()
startSoundChannelClient sc soundChannel synchronized client = do
    setSoundChannelClient soundChannel (SoundClientInitializing synchronized) client
    let mkChan = mkControlMessage sc "start" ["chan" .= soundChannel.id, "wid" .= soundChannel.winID]
    sendBinary client mkChan

startSoundChannelKeep :: (DisplayClient -> Bool) -> SoundCard -> WinID -> SoundChannelName -> STM SoundChannel
startSoundChannelKeep p sc wid name = do
    soundChannel <- NM.addWithKey sc.channels (newSoundChannel wid name)
    -- initialize clients already connected
    clients <- getClients sc.clients
    traverse_ (startSoundChannelClient sc soundChannel True) (filter p clients)
    broadcast sc.events (SoundChannelEvent soundChannel.id)
    pure soundChannel

startSoundChannel :: SoundCard -> WinID -> SoundChannelName -> STM SoundChannel
startSoundChannel = startSoundChannelKeep (const True)

stopSoundChannel :: SoundCard -> SoundChannel -> STM ()
stopSoundChannel sc soundChannel = do
    NM.delete sc.channels (from soundChannel.id)
    -- destroy the channel on the clients' side
    clients <- getClients sc.clients
    let delChan = mkControlMessage sc "stop" ["chan" .= soundChannel.id]
    forM_ clients $ \client -> do
        sendBinary client delChan
    broadcast sc.events (SoundChannelEvent soundChannel.id)

mkControlMessage :: SoundCard -> Text -> [Pair] -> LByteString
mkControlMessage sc op attrs = encodeMessageL sc.wid $ LBS.cons 0 buf
  where
    buf = encodeJSON (KM.fromList $ ["op" .= op] <> attrs)

lookupSoundChannel :: SoundCard -> SoundChannelID -> STM (Maybe SoundChannel)
lookupSoundChannel sc k = NM.lookup sc.channels (from k)

feedChannel :: SoundCard -> SoundChannel -> ByteString -> Maybe EBML.StreamFrame -> STM ()
feedChannel sc soundChannel buf mFrame = do
    let initSegments = from $ maybe mempty (\frame -> frame.initialization <> frame.media) mFrame
        mkBuffer = encodeMessageL sc.wid . encodeMessageL (from soundChannel.id)
        initBuffer = mkBuffer initSegments
        arrBuffer = mkBuffer (from buf)

    isRunning <- stateTVar soundChannel.frameCount \c -> (c > 0, c + 1)
    let sendData client isSynced
            | -- If the stream just started, or if the client is already synced
              not isRunning || isSynced =
                sendBinary client arrBuffer >> pure True
            | -- A new frame is available
              isJust mFrame =
                sendBinary client initBuffer >> pure True
            | otherwise = pure False

    clients <- getClients sc.clients
    forM_ clients \client -> do
        getSoundChannelClient soundChannel client >>= \case
            Nothing -> do
                let isSynced = not isRunning || isJust mFrame
                startSoundChannelClient sc soundChannel isSynced client
                void $ sendData client False
            Just (SoundClientInitializing isSynced) -> do
                newIsSynced <- sendData client isSynced
                when (newIsSynced /= isSynced) do
                    setSoundChannelClient soundChannel (SoundClientInitializing newIsSynced) client
            Just SoundClientPlaying -> sendBinary client arrBuffer
            _ -> pure ()

pauseChannel :: SoundCard -> SoundChannel -> STM ()
pauseChannel sc soundChannel = do
    writeTVar soundChannel.frameCount 0
    clients <- getClients sc.clients
    forM_ clients \client -> do
        getSoundChannelClient soundChannel client >>= \case
            Just SoundClientPlaying -> do
                sendBinary client (mkControlMessage sc "pause" ["chan" .= soundChannel.id])
            Just SoundClientMuted -> pure ()
            _ -> delSoundChannelClient soundChannel client

-------------------------------------------------------------------------------
-- Sound Receiver (for recorder)
-------------------------------------------------------------------------------
data SoundReceiver = SoundReceiver
    { client :: DisplayClient
    , stream :: TVar EBML.StreamReader
    , counter :: TVar Natural
    }

newSoundReceiver :: DisplayClient -> STM SoundReceiver
newSoundReceiver client = SoundReceiver client <$> newTVar EBML.newStreamReader <*> newTVar 0

delSoundReceiver :: SoundCard -> DisplayClient -> STM ()
delSoundReceiver sc client = do
    NM.delete sc.receivers (from client.process.pid)
    broadcast sc.events SoundReceiverStopped

getSoundReceiver :: SoundCard -> DisplayClient -> STM SoundReceiver
getSoundReceiver sc client =
    NM.lookup sc.receivers k >>= \case
        Nothing -> do
            newReceiver <- newSoundReceiver client
            NM.insert sc.receivers k newReceiver
            broadcast sc.events SoundReceiverStarted
            pure newReceiver
        Just x -> pure x
  where
    k = from client.process.pid

soundReceiverHandler :: SoundCard -> DisplayClient -> ByteString -> ProcessIO ()
soundReceiverHandler sc client buf = do
    receiver <- atomically (getSoundReceiver sc client)
    sr <- readTVarIO receiver.stream
    case EBML.feedReader buf sr of
        Left e -> do
            case buf of
                "" -> logInfo "receiver stopped" ["client" .= client]
                _ -> logError "received failed" ["client" .= client, "err" .= e]
            atomically (writeTVar receiver.stream EBML.newStreamReader)
        Right (mFrame, newSR) -> do
            atomically do
                writeTVar receiver.stream newSR
                modifyTVar' receiver.counter (+ 1)
            atomically $ broadcast sc.events (SoundReceiveEvent client buf mFrame)

startClientRecorder :: SoundCard -> WinID -> DisplayClient -> STM ()
startClientRecorder sc wid client = sendBinary client msg
  where
    msg = mkControlMessage sc "start-record" ["wid" .= wid]

stopSoundReceiver :: SoundCard -> WinID -> DisplayClient -> STM ()
stopSoundReceiver sc wid client = sendBinary client msg
  where
    msg = mkControlMessage sc "stop-record" ["wid" .= wid]

soundHandler :: SoundCard -> AppEvent -> ProcessIO ()
soundHandler sc = \case
    AppData de -> doSoundHandler sc de.client de.buffer
    ev -> logError "Unknown ev" ["ev" .= ev]

-- Simple binary protocol:
-- [0, op] : stop/start message
-- [0, chan, op] : error/ready/playing message
-- [data...] : audio data
doSoundHandler :: SoundCard -> DisplayClient -> ByteString -> ProcessIO ()
doSoundHandler sc client msg = do
    -- logDebug "Received audio data" ["client" .= client, "length" .= BS.length msg, "buf" .= BSLog (BS.take 5 msg)]
    case BS.uncons msg of
        Just (0, "\x00") -> do
            logInfo "audio client stopped" ["client" .= client]
            atomically $ delSoundClient sc client
        Just (0, "\x01") -> do
            logInfo "audio client started" ["client" .= client]
            atomically $ addSoundClient sc client
        Just (0, "\x02") -> do
            logInfo "audio receiver started" ["client" .= client]
        -- TODO: check that it's not already running
        Just (0, "\x03") -> do
            logInfo "audio receiver stopped" ["client" .= client]
            atomically $ delSoundReceiver sc client
        Just (0, bs) | BS.length bs == 2 -> case decodeMessage bs of
            Just (from -> channelID, ev) -> do
                mSoundChannel <- atomically (lookupSoundChannel sc channelID)
                case mSoundChannel of
                    Just soundChannel -> soundChannelHandler soundChannel ev
                    Nothing
                        | ev == "\x00" -> pure ()
                        | otherwise -> logError "unknown channel id" ["chan" .= channelID, "ev" .= BSLog ev]
            Nothing -> logError "the impossible has happened" ["ev" .= BSLog bs]
        _ -> soundReceiverHandler sc client msg
  where
    soundChannelHandler soundChannel ev = atomically do
        newStatus <- case ev of
            "\x00" -> pure (SoundClientError "Stopped")
            "\x01" -> pure SoundClientPlaying
            _ -> pure (SoundClientError "Unknown event received")
        setSoundChannelClient soundChannel newStatus client
        broadcast sc.events (SoundChannelEvent soundChannel.id)

-------------------------------------------------------------------------------
-- Client payload
-------------------------------------------------------------------------------
soundClient :: WinID -> Text
soundClient wid =
    [raw|
function setupSoundClient(chan) {
  // protocol helpers
  const sendPlaying = (achan) => butlerDataSocketSend(new Uint8Array([chan, 0, achan, 1]))
  const sendStopped = (achan) => butlerDataSocketSend(new Uint8Array([chan, 0, achan, 0]))
  const sendStoppedRec = () =>   butlerDataSocketSend(new Uint8Array([chan, 0, 3]))
  const sendRecording = () =>    butlerDataSocketSend(new Uint8Array([chan, 0, 2]))
  const sendConnected = () =>    butlerDataSocketSend(new Uint8Array([chan, 0, 1]))
  const sendDisconnected = () => butlerDataSocketSend(new Uint8Array([chan, 0, 0]))
  const sendAudioBuffer = (arr) => {
    let msg = new Uint8Array(1 + arr.length);
    msg[0] = chan;
    msg.set(arr, 1);
    butlerDataSocket.send(msg);
  }

  // display helpers
  const setHighlight = (elt) => {
    if (elt.classList.contains("bg-stone-400")) {
      elt.classList.remove("bg-stone-400");
    }
    elt.classList.add("bg-red-300");
  }
  const removeHighlight = (elt) => {
    if (elt.classList.contains("bg-red-300")) {
      elt.classList.remove("bg-red-300");
    }
    elt.classList.add("bg-stone-400");
  }
  const systemElt = document.getElementById("toggle-audio");

  const addWindowPlayingIcon = (player, wid) => {
    if (player.icon) {
      player.icon.classList.remove("hidden")
    } else {
      const winControlElt = document.querySelector("#win-" + wid + " div.wb-control")
      player.icon = document.createElement("i")
      player.icon.classList.add("ri-volume-up-line", "relative", "bottom-2")
      player.icon.onclick = () => player.stop()
      winControlElt.prepend(player.icon)
    }
  }

  const addWindowRecordingIcon = (wid) => {
    const winControlElt = document.querySelector("#win-" + wid + " div.wb-control")
    const icon = document.createElement("i")
    icon.classList.add("ri-mic-line", "relative", "bottom-2")
    icon.onclick = () => stopRecord(wid)
    winControlElt.prepend(icon)
    return icon
  }

  // Local state
  const butlerPlayers = {};
  globalThis.butlerPlayers = butlerPlayers

  const createPlayer = (achan, wid) => {
    const player = { stopped: false, playing: false, chunks: [], dst: null }

    const appendChunks = () => {
      if (player.chunks.length > 0) {
        if (!player.playing) {
          player.playing = true
          audio.play()
          sendPlaying(achan)
          playerWatchdog()
          addWindowPlayingIcon(player, wid)
        }
        player.dst.appendBuffer(concatBuffers(player.chunks))
        if (player.chunks.length > 10) {
          console.log("Player is getting late...", player.chunks.length)
        }
        player.chunks = []
      }
    }

    const onAudioError = err => {
      console.error("audio error", err)
      player.stop()
    }

    const audio = document.createElement("audio")
    audio.autoplay = true
    audio.onerror = onAudioError
    audio.onplay = ev => {
      console.log("audio play", ev)
    }

    const askAutoPlay = err => {
      console.error("play failed", err)
      if (!player.stopped) {
      alert(`AutoPlay is not available, Setup and reload with:
        - Press Ctrl-I
        - Click on the Permissions tab
        - Unmark the Audoplay 'Use Default' checkbox
        - Select the 'Allow Audio' toggle
      `)
      }
    }

    const playerWatchdog = () => {
      // Check that auto play is actually working, when stream is corrupted the player may get stuck
      setTimeout(() => {
        if (player.playing && !player.stopped && audio.currentTime < 1) {
          console.error("Audio failed to start", audio)
          player.stop()
        }
      }, 1500)
    }

    // Create the media source.
    const mediaSource = new MediaSource()

    mediaSource.onsourceopen = () => {
      // Create the source buffer.
      player.dst = mediaSource.addSourceBuffer("audio/webm; codecs=opus")
      player.dst.mode = "sequence"
      player.dst.onerror = onAudioError

      // If the buffer ends, flush any cached chunks.
      player.dst.onupdateend = appendChunks

      audio.play().then(() => {}, askAutoPlay)
    }
    audio.src = URL.createObjectURL(mediaSource)

    player.doStop = () => {
      player.stopped = true
      audio.pause()
      audio.currentTime = 0
      if (player.icon) {
        player.icon.remove()
      }
    }

    player.stop = () => {
      sendStopped(achan)
      player.doStop()
    }

    player.pause = () => {
      // audio.pause()
      player.playing = false
      if (player.icon) {
        player.icon.classList.add("hidden")
      }
      setTimeout(() => {
        if (!player.playing) {
          audio.pause()
        }
      }, 500)
      player.dst.abort()
      player.dst.changeType("audio/webm; codecs=opus")
      player.chunks = []
      console.log("Paused player", player)
    }

    player.feed = arr => {
      player.chunks.push(arr)
      if (player.dst && !player.dst.updating) {
        try {
          appendChunks()
        } catch (error) {
          onAudioError({reason: "Append failed", error})
        }
      }
    }

    player.audio = audio
    console.log("Created player", {player, achan})
    return player
  }

  const handleAudioControl = (ev) => {
    console.log("Got audio control", ev)
    const achan = ev["chan"]
    switch (ev["op"]) {
      case "start":
        if (butlerPlayers[achan]) {
          console.error("Player already exists", achan, butlerPlayers)
          butlerPlayers[achan].doStop()
        }
        butlerPlayers[achan] = createPlayer(achan, ev["wid"])
        break
      case "stop":
        butlerPlayers[achan].stop()
        delete butlerPlayers[achan]
        break
      case "pause":
        butlerPlayers[achan].pause()
        break
      case "start-record":
        startRecord(ev["wid"])
        break
      case "stop-record":
        stopRecord(ev["wid"])
        break
      default:
        console.error("Unknown audio control", ev)
    }
  }

  // Handlers for event received by the server
  butlerDataHandlers[chan] = buf => {
    const audioChan = buf[0];
    const audioBuf = buf.slice(1);
    if (audioChan === 0) {
      handleAudioControl(decodeJSON(audioBuf))
    } else {
      butlerPlayers[audioChan].feed(audioBuf);
    }
  }

  globalThis.butlerRecorder = { wins: {}, stopped: true }
  const isRecording = () => (Object.keys(butlerRecorder.wins).length > 0)
  const startRecord = (wid) => {
    butlerRecorder.stopped = false
    if (!isRecording()) {
      startRecorder(wid)
    }
    butlerRecorder.wins[wid] = addWindowRecordingIcon(wid)
  }

  const startRecorder = (wid) => {
    if (!navigator.mediaDevices.getUserMedia) {
      alert("navigator.mediaDevices.getUserMedia not available");
      return;
    }
    const onSuccess = (stream) => {
      const mediaRecorder = new MediaRecorder(stream, {mimeType: "audio/webm; codecs=opus"});
      mediaRecorder.ondataavailable = (e) => {
        // console.log("Recorder got samples", e);
        if (!butlerRecorder.stopped && e.data.size > 0) {
          e.data.arrayBuffer().then((buf) => {
            sendAudioBuffer(new Uint8Array(buf));
            // This may be the last frame of a stopped record
            stopRecordIfNeeded()
          })
        }
      }

      mediaRecorder.onerror = onRecError
      mediaRecorder.start(250);
      butlerRecorder.mediaRecorder = mediaRecorder
      butlerRecorder.stream = stream
      sendRecording()
    }
    const onRecError = (err) => {
      butlerRecorder.wins[wid].remove()
      sendStoppedRec()
      console.error('recordAudio error', err);
    }
    navigator.mediaDevices.getUserMedia({ audio: true, video: false }).then(onSuccess, onRecError);
  }

  const stopRecordIfNeeded = () => {
    if (!isRecording() && !butlerRecorder.stopped) {
      butlerRecorder.stopped = true
      sendStoppedRec()
    }
  }
  const stopRecord = (wid) => {
    butlerRecorder.wins[wid].remove()
    delete butlerRecorder.wins[wid]
    if (!isRecording()) {
      if (butlerRecorder.mediaRecorder) {
        butlerRecorder.mediaRecorder.stop();
        butlerRecorder.stream.getTracks().forEach(function(track) {
          track.stop();
        });
      }
      setTimeout(stopRecordIfNeeded, 1000)
    }
  }

  globalThis.stopAudio = () => {
    for (const [achan, player] of Object.entries(butlerPlayers)) {
      player.stop()
      delete butlerPlayers[achan]
    }
    for (const wid of Object.keys(butlerRecorder.wins)) {
      stopRecord(wid)
    }
    sendDisconnected()
  }
  globalThis.startAudio = sendConnected
}
  |]
        <> "\nsetupSoundClient("
        <> showT wid
        <> ");"
