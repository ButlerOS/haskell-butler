-- | This module contains the logic
module Butler.REPL where

import Butler.Core
import Butler.Core.Logger
import Butler.Core.Network
import Butler.Display.Session
import Butler.Prelude
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text

newtype REPL = REPL [Command]

data Command = Command
    { name :: Text
    , runCommand :: [Text] -> ProcessIO (Either Text Text)
    }

recoveryLink :: Sessions -> Command
recoveryLink sessions = Command "show-recovery" \args -> runExceptT do
    -- decode args
    username <- case args of
        [username] -> pure (UserName username)
        _ -> throwError "usage: show-recovery username"

    -- look for user
    matchingSessions <- atomically (lookupSessionByUser sessions username)
    case matchingSessions of
        Just session -> do
            -- return recovery
            recoveryID <- lift (getOrCreateRecover sessions session)
            pure $ "_recovery?recover=" <> from recoveryID
        Nothing -> throwError "Unknown username"

listSessions :: Sessions -> Command
listSessions sessions = Command "list-sessions" \args -> runExceptT do
    -- decode args
    case args of
        [] -> pure ()
        _ -> throwError "usage: list-sessions"

    xs <- Map.elems <$> lift (readTVarIO sessions.sessions)
    olines <- atomically $ forM xs \session -> do
        from <$> readTVar session.username

    pure $ Text.unlines olines

adminREPL :: Sessions -> REPL
adminREPL sessions = REPL [recoveryLink sessions, listSessions sessions]

runUnixREPL :: REPL -> ProcessIO Void
runUnixREPL (REPL commands) = do
    unixService "control.sock" \sock -> void $ spawnThread $ fix \loop -> do
        mRes <- runExceptT do
            -- Note: increase this if the command payload does not fit in 4k
            value <- do
                buf <- lift (sktRecv sock 4096)
                when (buf == mempty) do
                    throwError []
                case decodeJSON (from buf) of
                    Just v -> pure v
                    Nothing -> throwError ["Invalid payload" .= BSLog buf]
            (commandTxt, args) <- case value of
                (cmd : rest) -> pure (cmd, rest)
                _ -> throwError ["Missing command" .= value]
            case filter (\command -> command.name == commandTxt) commands of
                (command : _) ->
                    lift (command.runCommand args) >>= \case
                        Left err -> throwError ["Command failure" .= err]
                        Right v -> sktSendAll sock (from $ encodeJSON v)
                [] -> throwError ["Unknwon command" .= commandTxt]
        case mRes of
            Right () -> loop
            Left [] -> pure ()
            Left xs -> do
                sktSendAll sock (from $ encodeJSON $ object xs)
                loop
