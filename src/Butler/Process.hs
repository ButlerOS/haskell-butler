module Butler.Process (
    Pid (..),
    Process (..),
    processID,
    ProcessStatus (..),
    ExitReason (..),
    ProgramName (..),
) where

import Butler.Clock
import Butler.Prelude

newtype Pid = Pid Natural
    deriving (Show)
    deriving newtype (Serialise, ToJSON, Num, Enum, Eq, Ord, Real, Integral)

instance From Pid Natural where
    from (Pid n) = n

data Process = Process
    { pid :: Pid
    -- ^ the process id
    , childs :: TVar [Process]
    -- ^ the list of sub process
    , scope :: Scope
    -- ^ the processor scope
    , threadId :: ThreadId
    -- ^ the haskell rts id
    , thread :: Thread ExitReason
    -- ^ the ki thread reference
    , doneVar :: TMVar ()
    -- ^ a sync var to terminate the process ki scope
    , status :: TVar ProcessStatus
    -- ^ the process status
    , program :: ProgramName
    , createdAt :: Time
    }

instance ToJSON Process where
    toJSON p = object ["pid" .= p.pid, "program" .= p.program]

instance Show Process where
    show = from . processID

processID :: Process -> Text
processID p = do
    let ProgramName program = p.program
        Pid pid = p.pid
     in "<" <> from (show pid) <> ">" <> from program

data ExitReason = Killed | Exited | Crashed SomeException
    deriving (Show)

instance From ExitReason Text where
    from = from . show

instance ToJSON ExitReason where
    toJSON = String . from . show

instance Eq ExitReason where
    Killed == Killed = True
    Exited == Exited = True
    _ == _ = False

data ProcessStatus
    = Ready
    | Running
    | Stopped (Time, ExitReason)
    deriving (Eq, Show)

newtype ProgramName = ProgramName Text
    deriving (Show, Generic)
    deriving newtype (Ord, Eq, Semigroup, Serialise, IsString, FromJSON, ToJSON)

instance From Natural ProgramName where
    from = ProgramName . from . show

instance FromHttpApiData Pid where
    parseUrlPiece txt = case readMaybe (from txt) of
        Just x -> Right $ Pid x
        Nothing -> Left "bad pid"
