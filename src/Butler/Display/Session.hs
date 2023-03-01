module Butler.Display.Session (
    Sessions (..),
    Session (..),
    withSessions,
    newInvite,
    deleteInvite,
    deleteSession,
    isEmptySessions,
    lookupSession,
    checkInvite,
    createSession,
    UserName (..),
    isValidUserName,
    SessionID (..),
    InviteID (..),
) where

import Butler.Prelude

import Data.Aeson

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
    , username :: UserName
    }
    deriving (Eq, Generic, Serialise, ToJSON)

newInvite :: MonadIO m => Sessions -> m ()
newInvite sessions = do
    invite <- InviteID <$> liftIO UUID.nextRandom
    atomically $ modifyMemoryVar sessions.invitations (Map.insert invite [])

deleteInvite :: Sessions -> InviteID -> STM ()
deleteInvite sessions invite = modifyMemoryVar sessions.invitations (Map.delete invite)

deleteSession :: Sessions -> SessionID -> ProcessIO ()
deleteSession sessions sessionID = do
    dbExecute sessions.db "DELETE FROM sessions WHERE uuid = :uuid" [":uuid" := into @Text sessionID]
    atomically $ modifyTVar' sessions.sessions (Map.delete sessionID)

data Sessions = Sessions
    { db :: Database
    , sessions :: TVar (Map SessionID Session)
    , invitations :: MemoryVar (Map InviteID [SessionID])
    }

newtype InviteID = InviteID UUID
    deriving (Ord, Eq, Generic, Show)
    deriving (ToJSON, FromJSON, ToHtml, FromHttpApiData, Serialise) via JsonUID

newtype SessionID = SessionID UUID
    deriving (Ord, Eq, Generic, Show)
    deriving (ToJSON, FromJSON, ToHtml, FromHttpApiData, Serialise, Hashable) via JsonUID

instance From SessionID Text where
    from (SessionID uuid) = UUID.toText uuid

instance FromJWT SessionID
instance ToJWT SessionID

sessionsDB :: DatabaseMigration
sessionsDB = dbSimpleCreate "sessions" "uuid TEXT, username TEXT"

sessionsFromDB :: Database -> ProcessIO [(SessionID, Session)]
sessionsFromDB db = fmap mkSession <$> dbQuery db "select * from sessions" []
  where
    mkSession (uuid, username) =
        let sessionID = SessionID (fromMaybe (error "bad uuid?!") (UUID.fromText uuid))
            session = Session sessionID (UserName username)
         in (sessionID, session)

withSessions :: (Sessions -> ProcessIO a) -> ProcessIO a
withSessions cb = withDatabase "sessions" sessionsDB \db -> do
    sessions <- newTVarIO =<< Map.fromList <$> sessionsFromDB db
    invitations <- snd <$> newProcessMemory "invitations.bin" (pure mempty)
    cb $ Sessions db sessions invitations

isEmptySessions :: Sessions -> STM Bool
isEmptySessions sessions = Map.null <$> readTVar sessions.sessions

checkInvite :: Sessions -> InviteID -> STM Bool
checkInvite sessions inviteID = Map.member inviteID <$> readMemoryVar sessions.invitations

lookupSession :: Sessions -> SessionID -> STM (Maybe Session)
lookupSession sessions sessionID = Map.lookup sessionID <$> readTVar sessions.sessions

addSessionDB :: Sessions -> Session -> ProcessIO ()
addSessionDB sessions session = do
    dbExecute
        sessions.db
        "INSERT INTO sessions (uuid, username) VALUES (:uuid, :username)"
        [":uuid" := into @Text session.sessionID, ":username" := into @Text session.username]

createSession :: Sessions -> UserName -> Maybe InviteID -> ProcessIO (Maybe Session)
createSession sessions username mInvite = do
    sessionID <- SessionID <$> liftIO UUID.nextRandom
    mSession <- atomically do
        firstSession <- isEmptySessions sessions
        validInvite <- case mInvite of
            Just invite -> checkInvite sessions invite
            Nothing -> pure False
        when validInvite do
            let invite = fromMaybe (error "oops?") mInvite
                alter = \case
                    Just xs -> Just (sessionID : xs)
                    Nothing -> Just [sessionID]
            modifyMemoryVar sessions.invitations (Map.alter alter invite)
        if firstSession || validInvite
            then do
                let session = Session sessionID username
                modifyTVar' sessions.sessions (Map.insert session.sessionID session)
                pure (Just session)
            else pure Nothing

    forM_ mSession (addSessionDB sessions)
    pure mSession

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
