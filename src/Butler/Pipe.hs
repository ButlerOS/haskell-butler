-- | This module provides 'Pipe' and 'BroadcastChan' helpers.
module Butler.Pipe (
    -- * BroadcastChan
    BroadcastChan,
    newBroadcastChan,
    newReaderChan,
    broadcast,

    -- * Pipe
    Pipe,
    newPipe,
    readPipe,
    tryWritePipe,
) where

import Butler.Prelude

-- | A 'BroadcastChan' is a single-producer multi-consumer channel.
newtype BroadcastChan a = BroadcastChan
    { chan :: TChan a
    }

-- | Creates the producer end.
newBroadcastChan :: STM (BroadcastChan a)
newBroadcastChan = BroadcastChan <$> newBroadcastTChan

-- | Creates a new reader end.
newReaderChan :: BroadcastChan a -> STM (TChan a)
newReaderChan b = dupTChan b.chan

-- | Broadcast a message.
broadcast :: BroadcastChan a -> a -> STM ()
broadcast b = writeTChan b.chan

-- | A 'Pipe' is a bounded channel, analogous to pipe(2).
newtype Pipe a = Pipe
    { content :: TBQueue a
    }

newPipe :: STM (Pipe a)
newPipe = Pipe <$> newTBQueue 42

-- | 'readPipe' block until a message is available.
readPipe :: Pipe a -> STM a
readPipe pipe = readTBQueue pipe.content

-- | 'tryWritePipe' returns False when the message could not be delivered because the pipe is full.
tryWritePipe :: Pipe a -> a -> STM Bool
tryWritePipe pipe v = doWrite <|> pure False
  where
    doWrite = do
        writeTBQueue pipe.content v
        pure True
