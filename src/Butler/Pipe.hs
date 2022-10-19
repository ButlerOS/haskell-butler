module Butler.Pipe where

import Butler.Prelude

newtype BroadcastChan a = BroadcastChan
    { chan :: TChan a
    }

newBroadcastChan :: STM (BroadcastChan a)
newBroadcastChan = BroadcastChan <$> newBroadcastTChan

newReaderChan :: BroadcastChan a -> STM (TChan a)
newReaderChan b = dupTChan b.chan

broadcast :: BroadcastChan a -> a -> STM ()
broadcast b = writeTChan b.chan

newtype Pipe a = Pipe
    { content :: TMVar a
    }

newPipe :: STM (Pipe a)
newPipe = Pipe <$> newEmptyTMVar

readPipe :: Pipe a -> STM a
readPipe pipe = takeTMVar pipe.content

data PipeException = PipeException deriving (Show)
instance Exception PipeException

-- TODO: throw error after delay, e.g. when the reader dies
writePipe :: HasCallStack => Pipe a -> a -> STM ()
writePipe pipe v = do
    putResult <- tryPutTMVar pipe.content v
    if putResult
        then pure ()
        else error "pipe failed"
