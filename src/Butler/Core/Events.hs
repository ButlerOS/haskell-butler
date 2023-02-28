-- | Data types for internal events
module Butler.Core.Events (SystemEvent (..)) where

import Butler.Core.Process
import Butler.Prelude

data SystemEvent
    = ProcessCreated Process
    | ProcessStopped Process ExitReason
    | ProcessMessage ByteString Process Text
    | StorageSync Int
    | SystemReady
    | SystemCompleted
    | DaemonCrashed Process Int
    deriving (Show)

instance From SystemEvent Text where
    from s = case s of
        ProcessCreated p -> processID p <> " created"
        ProcessStopped p err -> processID p <> " exited " <> from err
        ProcessMessage loc p msg -> processID p <> "\t" <> decodeUtf8 loc <> "\t" <> msg
        _ -> from (show s)
