module Butler.Auth.OIDC (oIDCAuthApp, OIDCClientID (..), OIDCClientSecret (..), OIDCPublicURL (..)) where

import Lucid

import Butler.Core
import Butler.Display
import Butler.Display.GUI
import Butler.Display.Session
import Butler.Display.WebSocket
import Butler.Prelude
import Butler.Servant

import Crypto.Hash.SHA256 (hash)
import Data.Aeson (encode)
import Data.ByteString.Base64 qualified as B64
import Data.ByteString.Char8 qualified as B
import Data.ByteString.Lazy qualified as BSL
import Data.Map.Strict qualified as HM
import Network.HTTP.Client (Manager)
import Network.HTTP.Client.TLS (newTlsManager)
import Servant
import Servant.Auth.Server as SAS
import Servant.HTML.Lucid
import System.Random (genByteString)
import System.Random qualified as Random
import Web.OIDC.Client qualified as O

-- The API is defined using Servant named routes, see: https://www.tweag.io/blog/2022-02-24-named-routes/
-- When implementing the API, use 'ServantProcessIO' for the 'mode' type variable.

{- | The login flow goes as follow:

 * 1/ A new user starts with the indexRoute:
   * if they have a valid cookie, the user is given a websocket url.
   * otherwise they get redirected to the guestCallbackRoute
 * 2/ The guestCallbackRoute creates a new session and redirect to 1/
 * 3/ User can go to the loginRoute and be redirected to external idp.
 * 4/ On success, the user gets redirected back to the callbackRoute.
      At that point, the user may or may not have an existing session,
      And an existing session for this provider may or may not already exist.
      See the "getProviderSession" method below for the process.
-}
data LoginAPI mode = LoginAPI
    { indexRoute :: mode :- Get '[HTML] (Html ())
    , loginRoute :: mode :- "_login" :> Get '[JSON] NoContent
    , logoutRoute :: mode :- "_logout" :> Get '[HTML] AuthResp
    , deleteSessionRoute :: mode :- "_delete" :> Get '[HTML] AuthResp
    , callbackRoute ::
        mode
            :- "_cb"
                :> QueryParam "error" Text
                :> QueryParam "code" Text
                :> QueryParam "state" Text
                :> Get '[HTML] AuthResp
    , guestCallbackRoute :: mode :- "_guest_cb" :> Get '[HTML] AuthResp
    }
    deriving (Generic)

-- | Create or assign the provider session: the butler session attached to an external provider subject.
getProviderSession :: Sessions -> UserName -> SessionProvider -> Maybe Session -> ProcessIO Session
getProviderSession sessions username provider mCurrentSession = do
    let ctx = ["username" .= username, "provider" .= provider]
        logSession name session = name .= session.sessionID
    mProviderSession <- atomically (lookupSessionByProvider sessions provider)
    case (mCurrentSession, mProviderSession) of
        (Nothing, Nothing) -> do
            logDebug "Creating a new session" ctx
            newSession sessions (Just provider) username
        (Nothing, Just session) -> do
            logDebug "Assigning the existing provider session" (logSession "provider_session" session : ctx)
            pure session
        (Just currentSession, Nothing) -> do
            logDebug "Promoting the current session to the new provider" (logSession "current_session" currentSession : ctx)
            changeProvider sessions currentSession provider
            pure currentSession
        (Just currentSession, Just session) -> do
            logDebug "Switching to the existing provider session" (logSession "current_session" currentSession : logSession "provider_session" session : ctx)
            -- perhaps the session could be marked as disabled?
            deleteSession sessions currentSession.sessionID
            pure session

newtype OIDCClientID = OIDCClientID ByteString
newtype OIDCClientSecret = OIDCClientSecret ByteString
newtype OIDCPublicURL = OIDCPublicURL ByteString

data OIDCProviderConfig = OIDCProviderConfig
    { opIssuerURL :: Text
    , opClientID :: OIDCClientID
    , opClientSecret :: OIDCClientSecret
    , opAppPublicURL :: OIDCPublicURL
    , opUserClaim :: Maybe Text
    , opEnforceAuth :: Bool
    , opName :: Text
    }
data OIDCEnv = OIDCEnv
    { oidc :: O.OIDC
    , manager :: Manager
    , provider :: O.Provider
    , redirectUri :: ByteString
    , sessionStoreStorage :: MVar (HM.Map O.State O.Nonce)
    , providerConfig :: OIDCProviderConfig
    }

initOIDCEnv :: OIDCProviderConfig -> IO OIDCEnv
initOIDCEnv providerConfig@OIDCProviderConfig{..} = do
    manager <- newTlsManager
    provider <- O.discover opIssuerURL manager
    sessionStoreStorage <- newMVar HM.empty
    let redirectUri = B.dropWhileEnd (== '/') publicUrl <> "/_cb"
        oidc = O.setCredentials clientId clientSecret redirectUri (O.newOIDC provider)
    pure OIDCEnv{..}
  where
    OIDCClientID clientId = opClientID
    OIDCClientSecret clientSecret = opClientSecret
    OIDCPublicURL publicUrl = opAppPublicURL

data OIDCState = OIDCState
    { randomT :: Text
    , uri :: Maybe Text
    }
    deriving (Generic, Show)

instance FromJSON OIDCState

instance ToJSON OIDCState

-- | Generate a random fixed size string of 42 char base64 encoded
genRandomB64 :: IO ByteString
genRandomB64 = B64.encode . hash <$> genRandom

-- | Generate a random fixed size string of 1024 Bytes
genRandom :: IO ByteString
genRandom = do
    g <- Random.newStdGen
    let (bs, _ng) = genByteString 1024 g
    pure bs

mkSessionStore :: OIDCEnv -> Maybe O.State -> Maybe Text -> O.SessionStore IO
mkSessionStore OIDCEnv{sessionStoreStorage} stateM uriM = do
    let sessionStoreGenerate = do
            rb <- liftIO genRandomB64
            let s = OIDCState (decodeUtf8 rb) uriM
            pure (B64.encode $ BSL.toStrict $ Data.Aeson.encode s)
        sessionStoreSave = storeSave
        sessionStoreGet = storeGet
        sessionStoreDelete = case stateM of
            Just state' -> modifyMVar_ sessionStoreStorage $ \store -> pure $ HM.delete state' store
            Nothing -> pure ()
     in O.SessionStore{..}
  where
    storeSave :: O.State -> O.Nonce -> IO ()
    storeSave state' nonce = modifyMVar_ sessionStoreStorage $ \store -> pure $ HM.insert state' nonce store
    storeGet :: O.State -> IO (Maybe O.Nonce)
    storeGet state' = do
        store <- readMVar sessionStoreStorage
        let nonce = HM.lookup state' store
        pure nonce

loginServer :: Sessions -> (Html () -> Html ()) -> JWTSettings -> OIDCEnv -> AuthResult SessionID -> Maybe Workspace -> LoginAPI AsProcessIO
loginServer sessions mkIndexHtml jwtSettings oidcenv auth mWorkspace =
    LoginAPI
        { indexRoute
        , loginRoute
        , logoutRoute
        , deleteSessionRoute
        , callbackRoute
        , guestCallbackRoute
        }
  where
    callbackRoute :: Maybe Text -> Maybe Text -> Maybe Text -> ServantProcessIO AuthResp
    callbackRoute mErr mCode mState = case (mErr, mCode, mState) of
        (Just errorMsg, _, _) -> failCallback $ "Error from remote provider: " <> errorMsg
        (_, Nothing, _) -> failCallback "No code parameter given"
        (_, _, Nothing) -> failCallback "No state parameter given"
        (_, Just oauthCode, Just oauthState) -> do
            tokens :: O.Tokens Value <-
                liftIO $
                    O.getValidTokens
                        (mkSessionStore oidcenv (Just $ encodeUtf8 oauthState) Nothing)
                        (oidc oidcenv)
                        (manager oidcenv)
                        (encodeUtf8 oauthState)
                        (encodeUtf8 oauthCode)
            -- https://developers.google.com/identity/openid-connect/openid-connect#an-id-tokens-payload
            let uuid = tokens.idToken.sub
                mName = tokens.idToken.otherClaims ^? key "name" . _String
                username = UserName $ fromMaybe uuid mName
                provider = externalProvider "google" uuid
            -- lift $ logDebug "ID Token data" ["token" .= show tokens.idToken]
            withSession \mCurrentSession -> do
                userSession <- lift (getProviderSession sessions username provider mCurrentSession)
                setCookiesAndRedirect userSession
      where
        failCallback :: Text -> ServantProcessIO AuthResp
        failCallback msg = do
            lift do logError "bad callback" ["msg" .= msg]
            logAsGuest

    guestCallbackRoute :: ServantProcessIO AuthResp
    guestCallbackRoute = do
        session <- lift $ newSession sessions Nothing "guest"
        setCookiesAndRedirect session

    setCookiesAndRedirect :: Session -> ServantProcessIO AuthResp
    setCookiesAndRedirect session = liftIO do
        SAS.acceptLogin cookieSettings jwtSettings session.sessionID >>= \case
            Just r -> do
                let page = workspaceUrl mWorkspace
                pure $ r (script_ $ "window.location.href = " <> showT page)
            Nothing -> error "oops?!"

    withSession :: (Maybe Session -> ServantProcessIO a) -> ServantProcessIO a
    withSession cb = case auth of
        Authenticated sessionID -> do
            lift (atomically (lookupSession sessions sessionID)) >>= \case
                Just session -> cb (Just session)
                Nothing -> do
                    lift $ logError "Unknown session" ["id" .= sessionID]
                    cb Nothing
        _ -> do
            lift $ logError "Invalid sesssion" []
            cb Nothing

    logAsGuest :: ServantProcessIO a
    logAsGuest = throwRedirect $ workspaceUrl mWorkspace <> "_guest_cb"

    indexRoute :: ServantProcessIO (Html ())
    indexRoute = withSession \case
        Just _session -> pure $ mkIndexHtml (websocketHtml (workspaceUrl mWorkspace))
        Nothing -> logAsGuest

    loginRoute :: ServantProcessIO NoContent
    loginRoute = do
        redirectLocation <- liftIO genOIDCURL
        throwError $ err303{errHeaders = [("Location", redirectLocation)]}
      where
        genOIDCURL :: IO ByteString
        genOIDCURL = do
            loc <-
                O.prepareAuthenticationRequestUrl
                    (mkSessionStore oidcenv Nothing Nothing)
                    oidcenv.oidc
                    -- https://developers.google.com/identity/openid-connect/openid-connect#scope-param
                    [O.openId, O.profile]
                    mempty
            pure . B.pack $ show loc

    logoutRoute :: ServantProcessIO AuthResp
    logoutRoute =
        lift . pure . SAS.clearSession cookieSettings $
            script_ "window.location.href = \"_guest_cb\""

    deleteSessionRoute :: ServantProcessIO AuthResp
    deleteSessionRoute = do
        withSession $ \case
            Just session -> lift $ deleteSession sessions session.sessionID
            Nothing -> pure ()
        logoutRoute

oIDCAuthApp :: AuthContext -> Sessions -> OIDCPublicURL -> OIDCClientID -> OIDCClientSecret -> (Html () -> Html ()) -> ProcessIO AuthApplication
oIDCAuthApp authContext sessions publicUrl clientId clientSecret mkIndexHtml = do
    env <- ask
    oidcenv <-
        liftIO . initOIDCEnv $
            OIDCProviderConfig
                "https://accounts.google.com"
                clientId
                clientSecret
                publicUrl
                (Just "email")
                False
                "Test"
    let srv = loginServer sessions mkIndexHtml authContext.jwtSettings oidcenv
    let app = Servant.serveWithContextT (Proxy @(BaseAPI LoginAPI)) authContext.servantContext (toServantHandler env) (withBaseAPI srv)
    pure $ AuthApplication app
