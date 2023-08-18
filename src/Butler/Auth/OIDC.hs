module Butler.Auth.OIDC (oIDCAuthApp, OIDCClientID (..), OIDCClientSecret (..)) where

import Lucid

import Butler.Core
import Butler.Display
import Butler.Display.GUI
import Butler.Display.Session
import Butler.Display.WebSocket
import Butler.Prelude

import Crypto.Hash.SHA256 (hash)
import Data.Aeson (encode)
import Data.ByteString.Base64 qualified as B64
import Data.ByteString.Char8 qualified as B
import Data.ByteString.Lazy qualified as BSL
import Data.Map.Strict qualified as HM
import Data.Time (UTCTime (..), fromGregorian)
import Network.HTTP.Client (Manager)
import Network.HTTP.Client.TLS (newTlsManager)
import Servant
import Servant.Auth as SA
import Servant.Auth.Server as SAS
import Servant.HTML.Lucid
import Servant.Server.Generic (AsServer)
import System.Random (genByteString)
import System.Random qualified as Random
import Web.OIDC.Client qualified as O

type AuthResp = Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] (Html ())

-- Servant named routes, see: https://www.tweag.io/blog/2022-02-24-named-routes/
data LoginAPI mode = LoginAPI
    { indexRoute :: mode :- Get '[HTML] (Html ())
    , loginRoute :: mode :- "_login" :> Get '[JSON] NoContent
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

data WorkspaceAPI mode = WorkspaceAPI
    { withoutWorkspace :: mode :- NamedRoutes LoginAPI
    , withWorkspace :: mode :- Capture "workspace" Workspace :> NamedRoutes LoginAPI
    }
    deriving (Generic)

newtype AuthAPI mode = AuthAPI
    { authRoute :: mode :- Auth '[SA.JWT, SA.Cookie] SessionID :> NamedRoutes WorkspaceAPI
    }
    deriving (Generic)

newtype OIDCClientID = OIDCClientID ByteString
newtype OIDCClientSecret = OIDCClientSecret ByteString

data OIDCProviderConfig = OIDCProviderConfig
    { opIssuerURL :: Text
    , opClientID :: OIDCClientID
    , opClientSecret :: OIDCClientSecret
    , opAppPublicURL :: Text
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
    -- TODO ensure '/' before '_cb'
    let redirectUri = encodeUtf8 opAppPublicURL <> "_cb"
        oidc = O.setCredentials clientId clientSecret redirectUri (O.newOIDC provider)
    pure OIDCEnv{..}
  where
    OIDCClientID clientId = opClientID
    OIDCClientSecret clientSecret = opClientSecret

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

authServer :: OS -> Process -> Sessions -> (Html () -> Html ()) -> JWTSettings -> OIDCEnv -> AuthAPI AsServer
authServer os process sessions mkIndexHtml jwtSettings oidcenv =
    AuthAPI
        { authRoute = \auth ->
            WorkspaceAPI
                { withoutWorkspace = loginServer auth Nothing
                , withWorkspace = \ws -> loginServer auth (Just ws)
                }
        }
  where
    loginServer :: AuthResult SessionID -> Maybe Workspace -> LoginAPI AsServer
    loginServer auth mWorkspace =
        LoginAPI
            { indexRoute = indexRoute mWorkspace auth
            , loginRoute = loginRoute
            , callbackRoute = callbackRoute mWorkspace
            , guestCallbackRoute = guestCallbackRoute mWorkspace
            }

    callbackRoute :: Maybe Workspace -> Maybe Text -> Maybe Text -> Maybe Text -> Servant.Handler AuthResp
    callbackRoute mWorkspace mErr mCode mState = do
        case (mErr, mCode, mState) of
            (Just errorMsg, _, _) -> error $ "Error from remote provider: " <> show errorMsg
            (_, Nothing, _) -> error "No code parameter given"
            (_, _, Nothing) -> error "No state parameter given"
            (_, Just oauthCode, Just oauthState) -> do
                tokens :: O.Tokens Value <-
                    liftIO $
                        O.getValidTokens
                            (mkSessionStore oidcenv (Just $ encodeUtf8 oauthState) Nothing)
                            (oidc oidcenv)
                            (manager oidcenv)
                            (encodeUtf8 oauthState)
                            (encodeUtf8 oauthCode)
                setSession mWorkspace (UserName tokens.idToken.sub) $ SessionProvider "google"

    guestCallbackRoute :: Maybe Workspace -> Servant.Handler AuthResp
    guestCallbackRoute mWorkspace = setSession mWorkspace "guest" localProvider

    setSession :: Maybe Workspace -> UserName -> SessionProvider -> Servant.Handler AuthResp
    setSession mWorkspace username provider = do
        foundSessions <- atomically $ lookupSessionByUser sessions localProvider "guest"
        case foundSessions of
            [foundSession] -> do
                void $ liftIO $ runProcessIO os process $ changeUsername sessions foundSession provider username
                liftIO $ setCookiesAndRedirect foundSession
            [] -> do
                session <- liftIO $ runProcessIO os process $ newSession sessions provider username
                liftIO $ setCookiesAndRedirect session
            _ -> error "oops"
      where
        setCookiesAndRedirect session =
            SAS.acceptLogin cookieSettings jwtSettings session.sessionID >>= \case
                Just r -> do
                    let page = workspaceUrl mWorkspace
                    pure $ r (script_ $ "window.location.href = " <> showT page)
                Nothing -> error "oops?!"

    indexRoute :: Maybe Workspace -> AuthResult SessionID -> Servant.Handler (Html ())
    indexRoute mWorkspace = \case
        Authenticated sessionID -> do
            mSession <- atomically (lookupSession sessions sessionID)
            case mSession of
                Just _ ->
                    pure $ mkIndexHtml (websocketHtml (workspaceUrl mWorkspace))
                Nothing -> do
                    liftIO $ runProcessIO os process $ logError "no more auth?" ["id" .= sessionID]
                    pure "Unknown session"
        _ -> throwError $ err303{errHeaders = [("Location", encodeUtf8 (workspaceUrl mWorkspace) <> "_guest_cb")]}

    loginRoute :: Servant.Handler NoContent
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
                    [O.openId]
                    mempty
            pure . B.pack $ show loc

cookieSettings :: CookieSettings
cookieSettings =
    defaultCookieSettings
        { cookieSameSite = SameSiteStrict
        , cookieXsrfSetting = Nothing
        , cookieExpires = Just (UTCTime (fromGregorian 2030 1 1) 0)
        }

oIDCAuthApp :: Sessions -> Text -> OIDCClientID -> OIDCClientSecret -> (Html () -> Html ()) -> ProcessIO AuthApplication
oIDCAuthApp sessions public_url client_id client_secret mkIndexHtml = do
    JwkStorage myKey <- fst <$> newProcessMemory "display-key.jwk" (JwkStorage <$> liftIO generateKey)
    let jwtSettings = defaultJWTSettings myKey
    let cfg = cookieSettings :. jwtSettings :. EmptyContext
    env <- ask
    oidcenv <-
        liftIO . initOIDCEnv $
            OIDCProviderConfig
                "https://accounts.google.com"
                client_id
                client_secret
                public_url
                (Just "email")
                False
                "Test"
    let authSrv = authServer env.os env.process sessions mkIndexHtml jwtSettings oidcenv
    let app = Servant.serveWithContextT (Proxy @(NamedRoutes AuthAPI)) cfg id authSrv
    pure $ AuthApplication app
