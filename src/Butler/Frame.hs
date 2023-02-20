{- | Exchange format for API client.
 A frame contains html elements or binary data such as terminal output.
-}
module Butler.Frame (
    DataEvent (..),

    -- * helper
    encodeMessageL,
    decodeMessage,
    clientScript,
) where

import Butler.DisplayClient
import Butler.GUI
import Butler.Logger
import Butler.Prelude
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS

data DataEvent = DataEvent
    { client :: DisplayClient
    , buffer :: ByteString
    -- ^ The data buffer (without the channel id)
    , rawBuffer :: ByteString
    -- ^ The original buffer including the channel id. Use this to forward the message to other clients.
    }

instance ToJSON DataEvent where
    toJSON de = object ["client" .= de.client, "data" .= BSLog de.rawBuffer]

encodeMessageL :: WinID -> LByteString -> LByteString
encodeMessageL (WinID wid) = LBS.cons chan
  where
    chan
        | wid < 255 = unsafeFrom wid
        | otherwise = error $ "wid is too big " <> show wid

decodeMessage :: ByteString -> Maybe (WinID, ByteString)
decodeMessage buf = decodeChan <$> BS.uncons buf
  where
    decodeChan (x, xs) = (WinID (from x), xs)

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
globalThis.sendTrigger = (wid, name, obj) => {
  obj["HEADERS"] = {"HX-Trigger": withWID(wid, name)}
  butlerDataSocket.send(JSON.stringify(obj))
}

globalThis.withWID = (wid, name) => (name + "-" + wid);

globalThis.windows = {}
globalThis.onWindowResize = {}

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
