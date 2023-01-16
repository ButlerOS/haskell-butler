{- | Exchange format for API client.
 A frame contains html elements or binary data such as terminal output.

 TODO: integrate the htmx ext/ws.js . Presently the html data is managed by a dedicated websocket
-}
module Butler.Frame (
    ChannelID,
    newChannel,
    encodeMessage,
    decodeMessage,
    clientScript,
    winChannel,
) where

import Butler.Prelude
import Data.ByteString qualified as BS

newtype ChannelID = ChannelID Word8
    deriving newtype (Eq, ToJSON, Show)

instance From ChannelID Natural where
    from (ChannelID b) = from b

-- channelID are encoded as the first byte of the message, thus only 255 channels are presently available.
newChannel :: Natural -> ChannelID
newChannel n = case tryFrom n of
    Right b -> ChannelID b
    Left e -> error $ "Run out of channel: " <> show e

encodeMessage :: ChannelID -> ByteString -> ByteString
encodeMessage (ChannelID chan) = BS.cons chan

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
globalThis.butlerDataHandlers = {
  // 0 is getWindowSize()
  0: (() => butlerDataSocket.send(encodeDataMessage(0, {w: window.innerWidth, h: window.innerHeight})))
  // 1 is window manager
}
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
|]

-- | The channel for window data is reserved
winChannel :: ChannelID
winChannel = ChannelID 1
