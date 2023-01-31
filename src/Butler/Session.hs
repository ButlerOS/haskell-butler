module Butler.Session (
    Sessions (..),
    Session (..),
    loadSessions,
    newInvite,
    deleteInvite,
    deleteSession,
    isEmptySessions,
    checkSession,
    checkInvite,
    createSession,
    UserName (..),
    isValidUserName,
    SessionID (..),
    TabID (..),
    InviteID (..),
) where

import Butler.Prelude

import Data.Aeson
import Data.Hashable

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

import Butler.Memory
import Butler.OS
import Butler.User

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

deleteSession :: Sessions -> SessionID -> STM ()
deleteSession sessions session = modifyMemoryVar sessions.sessions (Map.delete session)

data Sessions = Sessions
    { sessions :: MemoryVar (Map SessionID Session)
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

loadSessions :: ProcessIO Sessions
loadSessions = do
    sessions <- snd <$> newProcessMemory "sessions.bin" (pure mempty)
    invitations <- snd <$> newProcessMemory "invitations.bin" (pure mempty)
    pure $ Sessions sessions invitations

isEmptySessions :: Sessions -> STM Bool
isEmptySessions (Sessions s _) = Map.null <$> readMemoryVar s

checkInvite :: Sessions -> InviteID -> STM Bool
checkInvite (Sessions _ invites) inviteID = Map.member inviteID <$> readMemoryVar invites

checkSession :: Sessions -> SessionID -> STM (Maybe Session)
checkSession (Sessions s _) sessionID = Map.lookup sessionID <$> readMemoryVar s

addSession :: Sessions -> SessionID -> Session -> STM Session
addSession (Sessions s _) k v = do
    modifyMemoryVar s (Map.insert k v)
    pure v

createSession :: MonadIO m => Sessions -> UserName -> Maybe InviteID -> m (Maybe Session)
createSession sessions username inviteM = do
    sessionID <- SessionID <$> liftIO UUID.nextRandom
    let session = Session sessionID username
    atomically do
        firstSession <- isEmptySessions sessions
        validInvite <- case inviteM of
            Just invite -> checkInvite sessions invite
            Nothing -> pure False
        when validInvite do
            let invite = fromMaybe (error "oops?") inviteM
                alter = \case
                    Just xs -> Just (sessionID : xs)
                    Nothing -> Just [sessionID]
            modifyMemoryVar sessions.invitations (Map.alter alter invite)
        if firstSession || validInvite
            then Just <$> addSession sessions sessionID session
            else pure Nothing

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
        Nothing -> fail "invalid uuid"

instance From JsonUID Text where
    from (JsonUID uuid) = UUID.toText uuid

instance ToHtml JsonUID where
    toHtml juid = toHtml (into @Text juid)

instance Serialise JsonUID where
    encode (JsonUID uuid) = encodeBytes (from (runPut (put uuid)))
    decode = fmap (JsonUID . decodeUUID) decodeBytes
      where
        decodeUUID = runGet get . from
