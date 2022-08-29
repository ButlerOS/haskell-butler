module Butler.App.REPL where

import Butler.History
import Butler.Prelude

data REPL = REPL
    { status :: TVar REPLStatus
    , history :: History REPLHistory
    }

data REPLHistory = REPLHistory
    { command :: Command
    , output :: ByteString
    }

newtype Command = Command Text

data REPLStatus = REPLReady | REPLWorking Command

newREPL :: STM REPL
newREPL = REPL <$> newTVar REPLReady <*> newHistory 42
