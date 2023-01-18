module Butler.SoundBlaster where

import Codec.EBML qualified as EBML
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types (Pair)
import Data.Binary.Put (putInt16le, runPut)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import GHC.Float (int2Float)

import Butler.Clock
import Butler.Display
import Butler.Frame
import Butler.GUI
import Butler.Logger
import Butler.NatMap qualified as NM
import Butler.Prelude
import Butler.Session
import Butler.User
import Butler.Window

-- Sound Card setup
data SoundCard = SoundCard
    { clients :: DisplayClients
    , channels :: NM.NatMap SoundChannel
    , receivers :: NM.NatMap SoundReceiver
    , events :: BroadcastChan SoundCardEvent
    }

newSoundCard :: STM SoundCard
newSoundCard = SoundCard <$> newDisplayClients <*> NM.newNatMap <*> NM.newNatMap <*> newBroadcastChan

data SoundCardEvent
    = SoundUserJoined
    | SoundUserLeft
    | SoundReceiverStarted
    | SoundReceiverStopped
    | SoundChannelEvent SoundChannelID
    | SoundReceiveEvent DisplayClient ByteString (Maybe EBML.StreamFrame)

addSoundClient :: SoundCard -> DisplayClient -> STM ()
addSoundClient sc client = do
    addClient sc.clients client

    -- attach to channels
    soundChannels <- NM.elems sc.channels
    traverse_ (flip startSoundChannelClient client) soundChannels

    broadcast sc.events SoundUserJoined

delSoundClient :: SoundCard -> DisplayClient -> STM ()
delSoundClient sc client = do
    delClient sc.clients client
    broadcast sc.events SoundUserLeft

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
    let
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
    drawList sc.channels "Playing:" (soundChannelHtml sc)
    drawList sc.receivers "Receiving:" soundReceiverHtml

soundChannelHtml :: SoundCard -> SoundChannel -> HtmlT STM ()
soundChannelHtml sc chan = do
    toHtml chan.name
    ": "
    clients <- lift (getClients sc.clients)
    forM_ clients \client -> do
        mClientStatus <- lift (getSoundChannelStatus chan client)
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

-- Sound Card channel (for player)
data SoundChannel = SoundChannel
    { winID :: WinID
    , name :: SoundChannelName
    , id :: SoundChannelID
    , statuses :: NM.NatMap SoundClientStatus
    }

newtype SoundChannelName = SoundChannelName Text
    deriving (Show, Generic)
    deriving newtype (Ord, Eq, Semigroup, Serialise, IsString, FromJSON, ToJSON, ToHtml)

newtype SoundChannelID = SoundChannelID ChannelID deriving newtype (Show, ToJSON)

instance From SoundChannelID Natural where
    from (SoundChannelID chan) = from chan

instance From SoundChannelID Word8 where
    from (SoundChannelID chan) = from chan

data SoundClientStatus
    = SoundClientInitializing
    | SoundClientReady
    | SoundClientSynchronizing
    | SoundClientSynchronized
    | SoundClientPlaying
    | SoundClientError Text

clientStatusHtml :: SoundClientStatus -> HtmlT STM ()
clientStatusHtml = \case
    SoundClientInitializing -> tooltip "(i)" "initializing..."
    SoundClientReady -> tooltip "(w)" "waiting for data..."
    SoundClientSynchronizing -> tooltip "..." "waiting for stream header..."
    SoundClientSynchronized -> tooltip "ooo" "waiting for play event"
    SoundClientPlaying -> ""
    SoundClientError e -> "[E: " <> toHtml e <> "]"
  where
    tooltip n title = with span_ [title_ title] n

newSoundChannel :: WinID -> SoundChannelName -> Natural -> STM SoundChannel
newSoundChannel wid name k = SoundChannel wid name (SoundChannelID $ newChannel k) <$> NM.newNatMap

setSoundChannelStatus :: SoundChannel -> SoundClientStatus -> DisplayClient -> STM ()
setSoundChannelStatus soundChannel status client = NM.insert soundChannel.statuses (from client.process.pid) status

getSoundChannelStatus :: SoundChannel -> DisplayClient -> STM (Maybe SoundClientStatus)
getSoundChannelStatus soundChannel client = NM.lookup soundChannel.statuses (from client.process.pid)

startSoundChannelClient :: SoundChannel -> DisplayClient -> STM ()
startSoundChannelClient soundChannel client = do
    setSoundChannelStatus soundChannel SoundClientInitializing client
    let mkChan = mkControlMessage "start" ["chan" .= soundChannel.id, "wid" .= soundChannel.winID]
    writeTChan client.sendChannel mkChan

startSoundChannel :: SoundCard -> WinID -> SoundChannelName -> STM SoundChannel
startSoundChannel sc wid name = do
    soundChannel <- NM.addWithKey sc.channels (newSoundChannel wid name)
    -- initialize clients already connected
    clients <- getClients sc.clients
    traverse_ (startSoundChannelClient soundChannel) clients
    broadcast sc.events (SoundChannelEvent soundChannel.id)
    pure soundChannel

stopSoundChannel :: SoundCard -> SoundChannel -> STM ()
stopSoundChannel sc soundChannel = do
    NM.delete sc.channels (from soundChannel.id)
    -- destroy the channel on the clients' side
    clients <- getClients sc.clients
    let delChan = mkControlMessage "stop" ["chan" .= soundChannel.id]
    forM_ clients $ \client -> do
        writeTChan client.sendChannel delChan
    broadcast sc.events (SoundChannelEvent soundChannel.id)

mkControlMessage :: Text -> [Pair] -> ByteString
mkControlMessage op attrs = from $ LBS.cons (from audioChannel) $ LBS.cons 0 buf
  where
    buf = encodeJSON (KM.fromList $ ["op" .= op] <> attrs)

lookupSoundChannel :: SoundCard -> SoundChannelID -> STM (Maybe SoundChannel)
lookupSoundChannel sc (SoundChannelID k) = NM.lookup sc.channels (from k)

feedChannel :: SoundCard -> SoundChannel -> ByteString -> Maybe EBML.StreamFrame -> STM ()
feedChannel sc soundChannel buf mFrame = do
    let initSegments = from $ maybe mempty (\frame -> frame.initialization <> frame.media) mFrame
        mkBuffer = LBS.cons (from audioChannel) . LBS.cons (from soundChannel.id)
        initBuffer = mkBuffer initSegments
        arrBuffer = mkBuffer (from buf)
    clients <- getClients sc.clients
    forM_ clients \client -> do
        getSoundChannelStatus soundChannel client >>= \case
            Just SoundClientReady
                | isNothing mFrame -> pure ()
                | otherwise -> do
                    writeTChan client.sendChannel (from initBuffer)
                    setSoundChannelStatus soundChannel SoundClientSynchronizing client
                    broadcast sc.events (SoundChannelEvent soundChannel.id)
            Just SoundClientSynchronizing -> do
                writeTChan client.sendChannel (from arrBuffer)
                setSoundChannelStatus soundChannel SoundClientSynchronized client
                broadcast sc.events (SoundChannelEvent soundChannel.id)
            Just SoundClientSynchronized -> do
                writeTChan client.sendChannel (from arrBuffer)
            Just SoundClientPlaying -> do
                writeTChan client.sendChannel (from arrBuffer)
            _ -> pure ()

-- Sound Card handler (for recorder)
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

startClientRecorder :: WinID -> DisplayClient -> STM ()
startClientRecorder wid client = writeTChan client.sendChannel msg
  where
    msg = mkControlMessage "start-record" ["wid" .= wid]

stopSoundReceiver :: WinID -> DisplayClient -> STM ()
stopSoundReceiver wid client = writeTChan client.sendChannel msg
  where
    msg = mkControlMessage "stop-record" ["wid" .= wid]

-- Simple binary protocol:
-- [0, op] : stop/start message
-- [0, chan, op] : error/ready/playing message
-- [data...] : audio data
soundHandler :: SoundCard -> ByteString -> ChannelID -> DisplayClient -> ByteString -> ProcessIO ()
soundHandler sc _rawBS _chan client msg = do
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
        Just (0, bs) | BS.length bs == 2 -> case BS.uncons bs of
            Just (SoundChannelID . from -> channelID, ev) -> do
                mSoundChannel <- atomically (lookupSoundChannel sc channelID)
                case mSoundChannel of
                    Just soundChannel -> soundChannelHandler soundChannel ev
                    Nothing
                        | ev == "\x00" -> pure ()
                        | otherwise -> logError "unknown channel id" ["chan" .= channelID, "ev" .= BSLog ev]
            Nothing -> logError "the impossible has happened" ["ev" .= BSLog bs]
        _ -> soundReceiverHandler sc client msg
  where
    soundChannelHandler soundChannel ev = do
        newStatus <- case ev of
            "\x00" -> pure (SoundClientError "Stopped")
            "\x01" -> pure SoundClientReady
            "\x02" -> pure SoundClientPlaying
            _ -> pure (SoundClientError "Unknown event received")
        atomically do
            setSoundChannelStatus soundChannel newStatus client
            broadcast sc.events (SoundChannelEvent soundChannel.id)

soundClient :: ChannelID -> Text
soundClient chan =
    [raw|
function setupSoundClient(chan) {
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
    const winControlElt = document.querySelector("#win-" + wid + " div.wb-control")
    player.icon = document.createElement("i")
    player.icon.classList.add("ri-volume-up-line", "relative", "bottom-2")
    player.icon.onclick = () => player.stop()
    winControlElt.prepend(player.icon)
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
          butlerDataSocketSend(new Uint8Array([chan, 0, achan, 2]))
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

    const audio = document.createElement("audio")
    audio.autoplay = true
    audio.onerror = e => console.error("audio error", {player, e, achan})
    audio.onplay = ev => {
      butlerDataSocketSend(new Uint8Array([chan, 0, achan, 1]))
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
        if (!player.stopped && audio.currentTime < 1) {
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
      player.dst.onerror = ev => console.error("source buffer error", {player, ev})

      // If the buffer ends, flush any cached chunks.
      player.dst.onupdateend = appendChunks

      audio.play().then(() => {}, askAutoPlay)
    }
    audio.src = URL.createObjectURL(mediaSource)

    player.stop = () => {
      player.stopped = true
      audio.pause()
      audio.currentTime = 0
      butlerDataSocketSend(new Uint8Array([chan, 0, achan, 0]))
      player.icon.remove()
    }

    player.feed = arr => {
      player.chunks.push(arr)
      if (player.dst && !player.dst.updating) {
        appendChunks()
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
          butlerPlayers[achan].stop()
        }
        butlerPlayers[achan] = createPlayer(achan, ev["wid"])
        break
      case "stop":
        butlerPlayers[achan].stop()
        delete butlerPlayers[achan]
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

  const sendAudioBuffer = (arr) => {
    let msg = new Uint8Array(1 + arr.length);
    msg[0] = chan;
    msg.set(arr, 1);
    butlerDataSocket.send(msg);
  }

  globalThis.butlerRecorder = { wins: {} }
  const isRecording = () => (Object.keys(butlerRecorder.wins).length > 0)
  const startRecord = (wid) => {
    if (!isRecording()) {
      startRecorder()
    }
    butlerRecorder.wins[wid] = addWindowRecordingIcon(wid)
  }

  const startRecorder = () => {
    if (!navigator.mediaDevices.getUserMedia) {
      alert("navigator.mediaDevices.getUserMedia not available");
      return;
    }
    const onSuccess = (stream) => {
      const mediaRecorder = new MediaRecorder(stream, {mimeType: "audio/webm; codecs=opus"});
      mediaRecorder.ondataavailable = (e) => {
        // console.log("Got samples", e);
        if (isRecording() && e.data.size > 0) {
          e.data.arrayBuffer().then((buf) => {
            sendAudioBuffer(new Uint8Array(buf));
          })
        }
      }

      mediaRecorder.onError = (e) => { console.error("media recorder", e); }
      mediaRecorder.start(250);
      butlerRecorder.mediaRecorder = mediaRecorder
      butlerRecorder.stream = stream
      butlerDataSocketSend(new Uint8Array([chan, 0, 2]))
    }
    const onError = (err) => {
      console.error('recordAudio error', err);
    }
    navigator.mediaDevices.getUserMedia({ audio: true, video: false }).then(onSuccess, onError);
  }
  const stopRecord = (wid) => {
    butlerRecorder.wins[wid].remove()
    delete butlerRecorder.wins[wid]
    if (!isRecording()) {
      butlerRecorder.mediaRecorder.stop();
      butlerRecorder.stream.getTracks().forEach(function(track) {
        track.stop();
      });
      butlerDataSocketSend(new Uint8Array([chan, 0, 3]))
    }
  }

  globalThis.stopAudio = () => {
    butlerDataSocketSend(new Uint8Array([chan, 0, 0]))
    for (const [achan, player] of Object.entries(butlerPlayers)) {
      player.stop()
      delete butlerPlayers[achan]
    }
  }
  globalThis.startAudio = () => {
    butlerDataSocketSend(new Uint8Array([chan, 0, 1]))
  }
}
  |]
        <> "\nsetupSoundClient("
        <> showT chan
        <> ");"

-- helper
testSound :: ByteString
testSound = encodeSampleList $ generateTone (round sampleRate) 440

renderNotes :: [Note] -> Float -> ByteString
renderNotes xs speed = encodeSampleList $ mconcat $ map (generateTone (round $ sampleRate / speed)) xs

generateTone :: Int -> Float -> [Int16]
generateTone size note = go 0
  where
    maxInt16 :: Int16
    maxInt16 = maxBound
    scale :: Float
    scale = int2Float (fromIntegral (maxInt16 `div` 5))
    sin16 :: Int -> Int16
    sin16 x = round $ (* scale) $ sin $ note * 2 * pi * int2Float x / sampleRate
    go :: Int -> [Int16]
    go n
        | n == size = []
        | otherwise = sin16 n : go (n + 1)

encodeSampleList :: [Int16] -> ByteString
encodeSampleList = from . runPut . put
  where
    put [] = pure ()
    put (x : xs) = putInt16le x >> put xs

sampleRate :: Float
sampleRate = 8000

recordTimeSlice :: Float
recordTimeSlice = 500

sampleLength :: ByteString -> Float
sampleLength buf = int2Float (BS.length buf `div` 2) / sampleRate

sampleLengthMS :: ByteString -> Milli
sampleLengthMS buf = Milli . round $ 1_000 * sampleLength buf

type Note = Float

furElise :: [Note]
furElise = [e4, ds4, e4, ds4, e4, b3, d4, c4, a3]

a3, as3, b3, c4, cs4, d4, ds4, e4, f4, fs4, g4, gs4, a4 :: Note
a3 = 222.00
as3 = 235.20
b3 = 249.19
c4 = 264.00
cs4 = 279.70
d4 = 296.33
ds4 = 313.96
e4 = 332.62
f4 = 352.40
fs4 = 373.36
g4 = 395.56
gs4 = 419.08
a4 = 440.00
