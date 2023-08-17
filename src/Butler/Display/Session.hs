module Butler.Display.Session (
    Sessions (..),
    Session (..),
    withSessions,
    newInvite,
    deleteInvite,
    deleteSession,
    isEmptySessions,
    lookupSession,
    lookupSessionByUser,
    checkInvite,
    newSession,
    changeUsername,
    UserName (..),
    isValidUserName,
    isUsernameAvailable,
    SessionID (..),
    InviteID (..),

    -- * Provider API
    SessionProvider,
    localProvider,
    externalProvider,

    -- * Recovery API
    getSessionFromRecover,
    getOrCreateRecover,
    deleteRecover,
    RecoveryID (..),

    -- * Permission API
    AdminSession,
    getAdminSession,
    withAdmin,
    setAdmin,
) where

import Butler.Prelude

import Codec.Serialise.Decoding (decodeBytes)
import Codec.Serialise.Encoding (encodeBytes)
import Data.Binary (Binary (get, put))
import Data.Binary.Get (runGet)
import Data.Binary.Put (runPut)
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import Servant.Auth.JWT (FromJWT, ToJWT)

import Butler.Core
import Butler.Core.Memory
import Butler.Core.Storage
import Butler.Database
import Butler.Display.User

-- | Where the session is coming from, e.g. local or external identity provider
newtype SessionProvider = SessionProvider Text
    deriving newtype (Eq)

instance From SessionProvider Text where from = coerce

-- | The default provider
localProvider :: SessionProvider
localProvider = SessionProvider "local"

externalProvider :: Text -> SessionProvider
externalProvider = SessionProvider

isValidUserName :: Text -> Maybe UserName
isValidUserName n
    | Text.null alphaName = Nothing
    | otherwise = Just (UserName alphaName)
  where
    alphaName = Text.takeWhile isAlphaNum n

data Session = Session
    { sessionID :: SessionID
    , provider :: TVar SessionProvider
    , username :: TVar UserName
    , admin :: TVar Bool
    , recover :: TVar (Maybe RecoveryID)
    }
    deriving (Generic)

instance ToJSON Session where
    toJSON s = toJSON s.sessionID

newInvite :: MonadIO m => Sessions -> InviteID -> m ()
newInvite sessions invite = do
    atomically $ modifyMemoryVar sessions.invitations (Map.insert invite [])

deleteInvite :: Sessions -> InviteID -> STM ()
deleteInvite sessions invite = modifyMemoryVar sessions.invitations (Map.delete invite)

deleteSession :: Sessions -> SessionID -> ProcessIO ()
deleteSession sessions sessionID = do
    dbExecute sessions.db "DELETE FROM sessions WHERE uuid = :uuid" [":uuid" := into @Text sessionID]
    atomically $ modifyTVar' sessions.sessions (Map.delete sessionID)

data Sessions = Sessions
    { db :: Database
    , lock :: MVar ()
    , sessions :: TVar (Map SessionID Session)
    , invitations :: MemoryVar (Map InviteID [SessionID])
    }

newtype InviteID = InviteID Text
    deriving (Ord, Eq, Generic, Show)
    deriving newtype (ToJSON, FromJSON, ToHtml, FromHttpApiData, Serialise, IsString)

newtype SessionID = SessionID UUID
    deriving (Ord, Eq, Generic, Show)
    deriving (ToJSON, FromJSON, ToHtml, FromHttpApiData, Serialise, Hashable) via JsonUID

newtype RecoveryID = RecoveryID UUID
    deriving (Ord, Eq, Generic, Show)
    deriving (ToJSON, FromJSON, ToHtml, FromHttpApiData, Serialise, Hashable) via JsonUID
instance ToField RecoveryID where toField = SQLText . from
instance From RecoveryID Text where from (RecoveryID uuid) = UUID.toText uuid

instance ToField SessionID where
    toField = SQLText . from

instance From SessionID Text where
    from (SessionID uuid) = UUID.toText uuid

instance From SessionID StorageAddress where
    from sess = StorageAddress $ encodeUtf8 $ into @Text sess

instance FromJWT SessionID
instance ToJWT SessionID

sessionsDB :: DatabaseMigration
sessionsDB = DatabaseMigration ["sessions-create", "admin", "recover", "provider"] doUp doDown
  where
    doUp name db = case name of
        "sessions-create" -> dbExecute db "CREATE TABLE sessions (uuid TEXT, username TEXT)" []
        "admin" -> do
            dbExecute db "ALTER TABLE sessions ADD COLUMN admin BOOLEAN" []
            dbExecute db "UPDATE sessions SET admin = FALSE" []
            dbExecute db "UPDATE sessions SET admin = TRUE WHERE rowid = 1" []
        "recover" -> dbExecute db "ALTER TABLE sessions ADD COLUMN recover TEXT" []
        "provider" -> do
            dbExecute db "ALTER TABLE sessions ADD COLUMN provider TEXT" []
            -- Set existing session to local
            dbExecute db "UPDATE sessions SET provider = :provider" [":provider" := into @Text localProvider]
        _ -> logError "Unknown migration" ["name" .= show name]
    doDown _ _ = pure ()

-- | TODO: only load active sessions when needed.
sessionsFromDB :: Database -> ProcessIO [(SessionID, Session)]
sessionsFromDB db = traverse mkSession =<< dbQuery db "select uuid,provider,username,admin,recover from sessions" []
  where
    mkSession (uuid, provider, username, admin, recoverTxt) = do
        let sessionID = SessionID (fromMaybe (error "bad uuid?!") (UUID.fromText uuid))
            recover = RecoveryID . fromMaybe (error "bad uuid!") . UUID.fromText <$> recoverTxt
        session <- Session sessionID <$> newTVarIO (SessionProvider provider) <*> newTVarIO (UserName username) <*> newTVarIO admin <*> newTVarIO recover
        pure (sessionID, session)

withSessions :: StorageAddress -> (Sessions -> ProcessIO a) -> ProcessIO a
withSessions addr cb = withDatabase addr sessionsDB \db -> do
    sessions <- newTVarIO =<< Map.fromList <$> sessionsFromDB db
    invitations <- snd <$> newProcessMemory "invitations.bin" (pure mempty)
    lock <- newMVar ()
    cb $ Sessions db lock sessions invitations

isEmptySessions :: Sessions -> STM Bool
isEmptySessions sessions = Map.null <$> readTVar sessions.sessions

checkInvite :: Sessions -> InviteID -> STM Bool
checkInvite sessions inviteID = Map.member inviteID <$> readMemoryVar sessions.invitations

lookupSession :: Sessions -> SessionID -> STM (Maybe Session)
lookupSession sessions sessionID = Map.lookup sessionID <$> readTVar sessions.sessions

lookupSessionByUser :: Sessions -> SessionProvider -> UserName -> STM [Session]
lookupSessionByUser sessions provider username = getSessions [] =<< (Map.elems <$> readTVar sessions.sessions)
  where
    getSessions :: [Session] -> [Session] -> STM [Session]
    getSessions acc [] = pure acc
    getSessions acc (session : rest) = do
        sessionProvider <- readTVar session.provider
        sessionUsername <- readTVar session.username
        let newAcc
                | sessionProvider == provider && sessionUsername == username = session : acc
                | otherwise = acc
        getSessions newAcc rest

getSessionFromRecover :: Sessions -> RecoveryID -> STM (Maybe Session)
getSessionFromRecover sessions uuid = do
    findSession =<< (Map.elems <$> readTVar sessions.sessions)
  where
    findSession = \case
        [] -> pure Nothing
        (session : rest) -> do
            mRecover <- readTVar session.recover
            case mRecover of
                Just recover | recover == uuid -> pure (Just session)
                _ -> findSession rest

getOrCreateRecover :: Sessions -> Session -> ProcessIO RecoveryID
getOrCreateRecover sessions session = do
    recoverID <- RecoveryID <$> liftIO UUID.nextRandom
    dbExecute sessions.db "UPDATE sessions SET recover = :recover WHERE uuid = :uuid" [":recover" := recoverID, ":uuid" := session.sessionID]
    atomically $ writeTVar session.recover (Just recoverID)
    pure recoverID

deleteRecover :: Sessions -> Session -> ProcessIO ()
deleteRecover sessions session = do
    atomically $ writeTVar session.recover Nothing
    dbExecute sessions.db "UPDATE sessions SET recover = NULL WHERE uuid = :uuid" [":uuid" := session.sessionID]

addSessionDB :: Sessions -> Session -> ProcessIO ()
addSessionDB sessions (Session sessionID tProvider tUsername tAdmin _) = do
    provider <- readTVarIO tProvider
    username <- readTVarIO tUsername
    admin <- readTVarIO tAdmin
    dbExecute
        sessions.db
        "INSERT INTO sessions (uuid, provider, username, admin) VALUES (:uuid, :provider, :username, :admin)"
        [":uuid" := sessionID, ":provider" := into @Text provider, ":username" := into @Text username, ":admin" := admin]

isUsernameAvailable :: Sessions -> SessionProvider -> UserName -> ProcessIO Bool
isUsernameAvailable sessions provider username = do
    null @[] @(Only Text)
        <$> dbQuery
            sessions.db
            "SELECT username from sessions WHERE provider = :provider AND username = :username"
            [":provider" := into @Text provider, ":username" := into @Text username]

changeUsername :: Sessions -> Session -> SessionProvider -> UserName -> ProcessIO Bool
changeUsername sessions session provider username
    | provider == localProvider && username == "guest" = pure False
    | otherwise = withMVar sessions.lock \() -> do
        avail <- isUsernameAvailable sessions provider username
        when avail doChangeUsername
        pure avail
  where
    doChangeUsername = do
        dbExecute
            sessions.db
            "UPDATE sessions SET username = :username, provider = :provider WHERE uuid = :uuid"
            [":username" := into @Text username, ":provider" := into @Text provider, ":uuid" := into @Text session.sessionID]
        atomically do
            writeTVar session.username username
            writeTVar session.provider provider

newSession :: Sessions -> SessionProvider -> UserName -> ProcessIO Session
newSession sessions provider username = do
    sessionID <- SessionID <$> liftIO UUID.nextRandom
    session <- atomically do
        isAdmin <- isEmptySessions sessions
        Session sessionID <$> newTVar provider <*> newTVar username <*> newTVar isAdmin <*> newTVar Nothing
    addSessionDB sessions session
    atomically $ modifyTVar' sessions.sessions (Map.insert session.sessionID session)
    pure session

newtype JsonUID = JsonUID UUID
    deriving newtype (Eq, Hashable)

instance FromJSON JsonUID where
    parseJSON = withText "UUID" \txt -> case UUID.fromText txt of
        Just uuid -> pure (JsonUID uuid)
        Nothing -> fail $ from $ "Invalid uuid: " <> txt

instance ToJSON JsonUID where
    toJSON (JsonUID uuid) = String (UUID.toText uuid)

instance FromHttpApiData JsonUID where
    parseUrlPiece txt = case UUID.fromText txt of
        Just uuid -> pure $ JsonUID uuid
        Nothing -> error "invalid uuid"

instance From JsonUID Text where
    from (JsonUID uuid) = UUID.toText uuid

instance ToHtml JsonUID where
    toHtml juid = toHtml (into @Text juid)

instance Serialise JsonUID where
    encode (JsonUID uuid) = encodeBytes (from (runPut (put uuid)))
    decode = fmap (JsonUID . decodeUUID) decodeBytes
      where
        decodeUUID = runGet get . from

newtype AdminSession = AdminSession {getAdminSession :: Session}

withAdmin :: Session -> (AdminSession -> ProcessIO a) -> ProcessIO ()
withAdmin session cb =
    readTVarIO session.admin >>= \case
        True -> void $ cb (AdminSession session)
        False -> logError "Permission denied" ["ses" .= session]

setAdmin :: Sessions -> AdminSession -> Session -> Bool -> ProcessIO ()
setAdmin sessions _ target admin = do
    dbExecute sessions.db "UPDATE sessions SET admin = :admin WHERE uuid = :sessionID" [":admin" := admin, ":sessionID" := target.sessionID]
    atomically $ writeTVar target.admin admin
