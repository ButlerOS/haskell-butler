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

isValidUserName :: Text -> Maybe UserName
isValidUserName n
    | Text.null alphaName = Nothing
    | otherwise = Just (UserName alphaName)
  where
    alphaName = Text.takeWhile isAlphaNum n

data Session = Session
    { sessionID :: SessionID
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
sessionsDB = DatabaseMigration ["sessions-create", "admin", "recover"] doUp doDown
  where
    doUp name db = case name of
        "sessions-create" -> dbExecute db "CREATE TABLE sessions (uuid TEXT, username TEXT)" []
        "admin" -> do
            dbExecute db "ALTER TABLE sessions ADD COLUMN admin BOOLEAN" []
            dbExecute db "UPDATE sessions SET admin = FALSE" []
            dbExecute db "UPDATE sessions SET admin = TRUE WHERE rowid = 1" []
        "recover" -> dbExecute db "ALTER TABLE sessions ADD COLUMN recover TEXT" []
        _ -> logError "Unknown migration" ["name" .= show name]
    doDown _ _ = pure ()

-- | TODO: only load active sessions when needed.
sessionsFromDB :: Database -> ProcessIO [(SessionID, Session)]
sessionsFromDB db = traverse mkSession =<< dbQuery db "select uuid,username,admin,recover from sessions" []
  where
    mkSession (uuid, username, admin, recoverTxt) = do
        let sessionID = SessionID (fromMaybe (error "bad uuid?!") (UUID.fromText uuid))
            recover = RecoveryID . fromMaybe (error "bad uuid!") . UUID.fromText <$> recoverTxt
        session <- Session sessionID <$> newTVarIO (UserName username) <*> newTVarIO admin <*> newTVarIO recover
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

lookupSessionByUser :: Sessions -> UserName -> STM [Session]
lookupSessionByUser sessions username = getSessions [] =<< (Map.elems <$> readTVar sessions.sessions)
  where
    getSessions :: [Session] -> [Session] -> STM [Session]
    getSessions acc [] = pure acc
    getSessions acc (session : rest) = do
        sessionUsername <- readTVar session.username
        let newAcc
                | sessionUsername == username = session : acc
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
addSessionDB sessions (Session sessionID tUsername tAdmin _) = do
    username <- readTVarIO tUsername
    admin <- readTVarIO tAdmin
    dbExecute
        sessions.db
        "INSERT INTO sessions (uuid, username, admin) VALUES (:uuid, :username, :admin)"
        [":uuid" := sessionID, ":username" := into @Text username, ":admin" := admin]

isUsernameAvailable :: Sessions -> UserName -> ProcessIO Bool
isUsernameAvailable sessions username = do
    null @[] @(Only Text)
        <$> dbQuery
            sessions.db
            "SELECT username from sessions WHERE username = :username"
            [":username" := into @Text username]

changeUsername :: Sessions -> Session -> UserName -> ProcessIO Bool
changeUsername _ _ "guest" = pure False
changeUsername sessions session username = withMVar sessions.lock \() -> do
    avail <- isUsernameAvailable sessions username
    when avail doChangeUsername
    pure avail
  where
    doChangeUsername = do
        dbExecute
            sessions.db
            "UPDATE sessions SET username = :username WHERE uuid = :uuid"
            [":username" := into @Text username, ":uuid" := into @Text session.sessionID]
        atomically $ writeTVar session.username username

newSession :: Sessions -> UserName -> ProcessIO Session
newSession sessions username = do
    sessionID <- SessionID <$> liftIO UUID.nextRandom
    session <- atomically do
        isAdmin <- isEmptySessions sessions
        Session sessionID <$> newTVar username <*> newTVar isAdmin <*> newTVar Nothing
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
