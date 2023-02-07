{- | Exchange format for API client.
 A frame contains html elements or binary data such as terminal output.

 TODO: integrate the htmx ext/ws.js . Presently the html data is managed by a dedicated websocket
-}
module Butler.Frame (
    -- * channel
    ChannelID,
    newChannel,

    -- * handler
    DataHandlers,
    newDataHandlers,
    newDataHandler,
    reserveDataHandlers,
    withDataHandler,
    lookupDataHandler,
    DataEvent (..),

    -- * helper
    encodeMessage,
    encodeMessageL,
    decodeMessage,
    clientScript,
    winChannel,
    audioChannel,
) where

import Butler.DisplayClient
import Butler.NatMap qualified as NM
import Butler.Pipe
import Butler.Prelude
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS

newtype ChannelID = ChannelID Word8
    deriving newtype (Eq, Ord, ToJSON, Show)

instance From ChannelID Natural where
    from (ChannelID b) = from b

instance From Word8 ChannelID where
    from = ChannelID

instance From ChannelID Word8 where
    from (ChannelID c) = c

-- channelID are encoded as the first byte of the message, thus only 255 channels are presently available.
newChannel :: Natural -> ChannelID
newChannel n = case tryFrom n of
    Right b -> ChannelID b
    Left e -> error $ "Run out of channel: " <> show e

newtype DataHandlers = DataHandlers (NM.NatMap (Pipe DataEvent))

-- | DataHandlers channel starts at 1
reserveDataHandlers :: DataHandlers -> Natural -> STM ()
reserveDataHandlers (DataHandlers nm) = go
  where
    go 0 = error "First handler (0) is always reserved"
    go 1 = pure ()
    go n = do
        -- Increase natmap key counter
        void $ NM.newKey nm
        go (n - 1)

newDataHandlers :: STM DataHandlers
newDataHandlers = DataHandlers <$> NM.newNatMap

lookupDataHandler :: DataHandlers -> ChannelID -> STM (Maybe (Pipe DataEvent))
lookupDataHandler (DataHandlers nm) chan = NM.lookup nm (from chan)

newDataHandler :: DataHandlers -> STM (ChannelID, Pipe DataEvent)
newDataHandler (DataHandlers nm) = do
    pipe <- newPipe
    chan <- newChannel <$> NM.add nm pipe
    pure (chan, pipe)

withDataHandler :: MonadUnliftIO m => DataHandlers -> (ChannelID -> Pipe DataEvent -> m a) -> m a
withDataHandler dh@(DataHandlers nm) cb = do
    (chan, pipe) <- atomically (newDataHandler dh)
    cb chan pipe `finally` do
        atomically (NM.delete nm (from chan))

data DataEvent = DataEvent
    { client :: DisplayClient
    , buffer :: ByteString
    , rawBuffer :: ByteString
    }

encodeMessage :: ChannelID -> ByteString -> ByteString
encodeMessage (ChannelID chan) = BS.cons chan

encodeMessageL :: ChannelID -> LByteString -> LByteString
encodeMessageL (ChannelID chan) = LBS.cons chan

decodeMessage :: ByteString -> Maybe (ChannelID, ByteString)
decodeMessage buf = decodeChan <$> BS.uncons buf
  where
    decodeChan (x, xs) = (newChannel (from x), xs)

clientScript :: Text
clientScript =
    [raw|
// Helpers
globalThis.decodeJSON = buf => JSON.parse(new TextDecoder().decode(buf));
globalThis.encodeJSON = obj => (new TextEncoder()).encode(JSON.stringify(obj));
globalThis.encodeDataMessage = (chan, obj) => {
  let dataBuf = encodeJSON(obj)
  let buf = new Uint8Array(1 + dataBuf.length);
  buf[0] = chan;
  buf.set(new Uint8Array(dataBuf), 1)
  return buf
}
globalThis.withWID = (wid, name) => (name + "-" + wid);

globalThis.windows = {}
globalThis.onWindowResize = {}

globalThis.butlerDataSocket = new WebSocket(wsUrl("data"));
globalThis.butlerDataHandlers = {}

butlerDataSocket.binaryType = 'arraybuffer';
globalThis.debounceData = (delay, handler) => {
  let timer;
  let lastEv;
  return (...ev) => {
    lastEv = ev
    if (timer === undefined) {
      timer = setTimeout(() => {
        msg = handler.apply(null, lastEv)
        if (msg) {
          butlerDataSocket.send(msg.buffer)
        }
        clearTimeout(timer);
        timer = undefined;
      }, delay);
    }
  };
}
butlerDataSocket.onerror = e => {
  console.log("data skt error", e)
}
butlerDataSocket.onmessage = event => {
  const buf = new Uint8Array(event.data)
  butlerDataHandlers[buf[0]](buf.slice(1))
}
globalThis.butlerDataSocketSend = (buf) => {
  if (butlerDataSocket.readyState != 1) {
    setTimeout(() => { butlerDataSocket.send(buf); }, 1000);
  } else {
    butlerDataSocket.send(buf)
  }
}

globalThis.concatBuffers = (xs) => {
  if (xs.length == 1) {
    return xs[0]
  }
  const size = xs.reduce((acc, x) => acc + x.length, 0);
  const res = new Uint8Array(size);
  let pos = 0;
  xs.forEach(x => {
    res.set(x, pos);
    pos += x.length
  })
  return res;
}
|]

-- | The channel for window and audio data are reserved
winChannel, audioChannel :: ChannelID
winChannel = ChannelID 1
audioChannel = ChannelID 0
