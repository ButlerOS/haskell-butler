module Butler.SoundBlaster where

import Data.Binary.Put (putInt16le, runPut)
import Data.ByteString qualified as BS
import GHC.Float (int2Float)

import Butler.Display
import Butler.Frame
import Butler.Logger
import Butler.OS
import Butler.Prelude

data SoundCard = SoundCard
    { clients :: DisplayClients
    , -- note: instead of a TMVar, this channel could be hardcoded like winChannel?
      chan :: TMVar ChannelID
    }

newSoundCard :: STM SoundCard
newSoundCard = SoundCard <$> newDisplayClients <*> newEmptyTMVar

delSoundClient :: SoundCard -> DisplayClient -> STM ()
delSoundClient sc = delClient sc.clients

addSoundClient :: SoundCard -> DisplayClient -> STM ()
addSoundClient sc = addClient sc.clients

playSample :: SoundCard -> ByteString -> ProcessIO ()
playSample sc bs = do
    chan <- atomically (readTMVar sc.chan)
    let buf = encodeMessage chan $ BS.cons 0 bs
    clientsBroadcastMessage sc.clients buf

registerSoundChannel :: SoundCard -> Handlers -> STM ChannelID
registerSoundChannel sc handlers = do
    chan <- newHandler handlers (soundHandler sc)
    putTMVar sc.chan chan
    pure chan

soundHandler :: SoundCard -> ByteString -> ChannelID -> DisplayClient -> ByteString -> ProcessIO ()
soundHandler sc rawBS _chan client msg = case BS.uncons msg of
    Just (0, "") -> do
        logInfo "audio client stopped" ["client" .= client]
        atomically $ delSoundClient sc client
    Just (1, "") -> do
        logInfo "audio client started" ["client" .= client]
        atomically $ addSoundClient sc client
    _ -> do
        logTrace "got audio event" ["ev" .= BSLog rawBS]

soundClient :: ChannelID -> Text
soundClient chan =
    [raw|
function setupSoundClient(chan) {
  // Local state
  const butlerPlayers = {};

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

  const waitComplete = (player, elt) => {
    setTimeout(() => {
      if (player.startTime > player.audioCtx.currentTime) {
        waitComplete(player, elt);
      } else {
        removeHighlight(elt);
      }
    }, 1000)
  };

  const playerConfig = (audioChan) => {switch (audioChan) {
    default: return {encoding: '16bitInt', channels: 1, sampleRate: 8000, flushingTime: 100}
  }};

  const systemElt = document.getElementById("toggle-audio");

  // Handlers for event received by the server
  butlerDataHandlers[chan] = buf => {
    const audioChan = buf[0];
    const audioBuf = buf.slice(1);
    // console.log("Got pcm frame", audioChan, audioBuf.length)
    if (audioChan === 0) {
      if (butlerPlayers[audioChan] === undefined) {
        butlerPlayers[audioChan] = new PCMPlayer(playerConfig(audioChan));
      }
      butlerPlayers[audioChan].feed(audioBuf);
      setHighlight(systemElt)
      waitComplete(butlerPlayers[audioChan], systemElt);
    }
  }
}
  |]
        <> "\nsetupSoundClient("
        <> showT chan
        <> ");"

startSoundClient :: ChannelID -> Text
startSoundClient chan = "butlerDataSocketSend(new Uint8Array([" <> showT chan <> ", 1]));"

stopSoundClient :: ChannelID -> Text
stopSoundClient chan = "butlerDataSocketSend(new Uint8Array([" <> showT chan <> ", 0]));"

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
