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
    { content :: TBQueue a
    }

newPipe :: STM (Pipe a)
newPipe = Pipe <$> newTBQueue 42

readPipe :: Pipe a -> STM a
readPipe pipe = readTBQueue pipe.content

readPipe2 :: Pipe a -> Pipe b -> STM (Either a b)
readPipe2 p1 p2 = (Left <$> readPipe p1) <|> (Right <$> readPipe p2)

readPipe3 :: Pipe a -> Pipe b -> Pipe c -> STM (Either a (Either b c))
readPipe3 p1 p2 p3 = (Left <$> readPipe p1) <|> (Right . Left <$> readPipe p2) <|> (Right . Right <$> readPipe p3)

data PipeException = PipeException deriving (Show)
instance Exception PipeException

tryWritePipe :: Pipe a -> a -> STM Bool
tryWritePipe pipe v = doWrite <|> pure False
  where
    doWrite = do
      writeTBQueue pipe.content v
      pure True
