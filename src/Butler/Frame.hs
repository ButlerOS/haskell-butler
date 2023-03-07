{- | Exchange format for API client.
 A frame contains html elements or binary data such as terminal output.
-}
module Butler.Frame (
    DataEvent (..),

    -- * helper
    encodeMessageL,
    encodeMessage,
    decodeMessage,
    butlerHelpersScript,
) where

import Butler.Core.Logger
import Butler.Display.Client
import Butler.Display.GUI
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
encodeMessageL wid = encodeMessage (from wid)

encodeMessage :: Natural -> LByteString -> LByteString
encodeMessage chan = LBS.cons b
  where
    b
        | chan < 255 = unsafeFrom chan
        | otherwise = error "Chan > 255 not implemented"

decodeMessage :: ByteString -> Maybe (WinID, ByteString)
decodeMessage buf = decodeChan <$> BS.uncons buf
  where
    decodeChan (x, xs) = (WinID (from x), xs)

butlerHelpersScript :: Text
butlerHelpersScript =
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

// Send a binary message.
globalThis.sendBinaryMessage = (chan, arr) => {
    let msg = new Uint8Array(1 + arr.length);
    msg[0] = chan
    msg.set(arr, 1);
    butlerDataSocket.send(msg);
}

// Send a binary message with 2 channels.
globalThis.sendBinaryMessage2 = (c1, c2, arr) => {
    let msg = new Uint8Array(2 + arr.length);
    msg[0] = c1
    msg[1] = c2
    msg.set(arr, 2);
    butlerDataSocket.send(msg);
}
globalThis.decodeDataMessage = (buf, cb) => {
  if (buf.length > 0) {
    const chan = buf[0]
    const tail = buf.slice(1)
    cb(chan, tail)
  } else {
    console.error("Invalid data message", buf)
  }
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
