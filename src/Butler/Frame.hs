{- | Exchange format for API client.
 A frame contains html elements or binary data such as terminal output.

Note: [Binary Message]

Application may send or receive binary payload using the following message encoding:

 | app-id | data      |

For HTMX trigger, the app-id is appended to the trigger name.
For binary message, the app-id is encoded at the begining of the payload using the
variable length int codec of EBML.
-}
module Butler.Frame (
    DataEvent (..),

    -- * helper
    encodeMessage,
    decodeMessage,
    butlerHelpersScript,
) where

import Butler.Core.Logger
import Butler.Display.Client
import Butler.Prelude
import Codec.EBML qualified as EBML
import Data.Binary.Get (runGetOrFail)
import Data.Binary.Put (putLazyByteString, runPut)

data DataEvent = DataEvent
    { client :: DisplayClient
    , buffer :: LByteString
    -- ^ The data buffer (without the channel id)
    , rawBuffer :: LByteString
    -- ^ The original buffer including the channel id. Use this to forward the message to other clients.
    }

instance ToJSON DataEvent where
    toJSON de = object ["client" .= de.client, "data" .= LBSLog de.rawBuffer]

encodeMessage :: Natural -> LByteString -> LByteString
encodeMessage chan buf = runPut do
    -- Using unsafeFrom will raise an error when the channel number is greater than a Word64.
    -- This should not happened before reaching a limitation somewhere else in the system.
    EBML.putDataSize (unsafeFrom chan)
    putLazyByteString buf

decodeMessage :: LByteString -> Maybe (Natural, LByteString)
decodeMessage buf = case runGetOrFail EBML.getDataSize buf of
    Left _ -> Nothing
    Right (rest, _, chan) -> Just (from chan, rest)

butlerHelpersScript :: Text
butlerHelpersScript =
    [raw|
// Helpers to convert between json and bytes
globalThis.decodeJSON = buf => JSON.parse(new TextDecoder().decode(buf));
globalThis.encodeJSON = obj => (new TextEncoder()).encode(JSON.stringify(obj));

// Helpers to handle variable int, adapted from node-ebml tools
// https://github.com/node-ebml/node-ebml/blob/master/src/ebml/tools.js
globalThis.readVint = (buffer, cb) => {
  const length = 8 - Math.floor(Math.log2(buffer[0]));
  if (length < 1 || length > 8) {
    throw ("Bad var int: " + buffer)
  }
  let value = buffer[0] & ((1 << (8 - length)) - 1);
  for (let i = 1; i < length; i += 1) {
    value *= 2 ** 8;
    value += buffer[i];
  }
  cb(value, buffer.slice(length))
}
globalThis.writeVint = (value) => {
  let length = 1;
  for (length = 1; length <= 8; length += 1) {
    if (value < 2 ** (7 * length) - 1) {
      break;
    }
  }
  const buffer = new Uint8Array(length);
  let val = value;
  for (let i = 1; i <= length; i += 1) {
    const b = val & 0xff;
    buffer[length - i] = b;
    val -= b;
    val /= 2 ** 8;
  }
  buffer[0] |= 1 << (8 - length);
  return buffer;
}

// Hooks to subscribe and receive binary event from htmx ws-ext
globalThis.butlerDataHandlers = {};
globalThis.butlerOnData = arr => {
  if (arr.length > 0) {
    readVint(arr, (chan, buf) => {
      const hdl = butlerDataHandlers[chan]
      if (hdl) {
        hdl(buf)
      } else {
        console.error("Unknown butler data", arr)
      }
    })
  } else {
    console.error("Invalid data message", arr)
  }
}

// Send JSON through a binary message.
globalThis.sendJSONMessage = (chan, obj) => {
  sendBinaryMessage(chan, encodeJSON(obj))
}

// Send a binary message.
globalThis.sendBinaryMessage = (chan, arr) => {
    let chanBuf = writeVint(chan)
    let msg = new Uint8Array(chanBuf.length + arr.length)
    msg.set(chanBuf, 0)
    msg.set(arr, chanBuf.length)
    butlerDataSocket.send(msg)
}

// Send a binary message with 2 channels.
globalThis.sendBinaryMessage2 = (c1, c2, arr) => {
    let c1Buf = writeVint(c1)
    let c2Buf = writeVint(c2)
    let msg = new Uint8Array(c1Buf.length + c2Buf.length + arr.length);
    msg.set(c1Buf, 0)
    msg.set(c2Buf, c1Buf.length)
    msg.set(arr, c1Buf.length + c2Buf.length);
    butlerDataSocket.send(msg);
}

// Send a trigger message
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
        handler.apply(null, lastEv)
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
