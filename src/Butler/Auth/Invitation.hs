module Butler.Auth.Invitation (invitationAuthApp) where

import Data.Map.Strict qualified as Map
import Data.Time (UTCTime (..), fromGregorian)
import Lucid
import Lucid.Htmx
import Servant
import Servant.Auth as SA
import Servant.Auth.Server as SAS
import Servant.HTML.Lucid
import Web.FormUrlEncoded (FromForm)

import Butler.Display
import Butler.GUI
import Butler.OS
import Butler.Prelude
import Butler.Session
import Butler.WebSocket

type AuthResp = Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] (Html ())

type LoginAPI =
    QueryParam "invite" InviteID :> Get '[HTML] (Html ())
        :<|> "login" :> ReqBody '[FormUrlEncoded] LoginForm :> Post '[HTML] AuthResp

type AuthAPI = Auth '[SA.JWT, SA.Cookie] SessionID :> (LoginAPI :<|> Capture "workspace" Workspace :> LoginAPI)

data LoginForm = LoginForm
    { invite :: Maybe InviteID
    , username :: UserName
    }
    deriving (Generic, FromJSON, ToJSON)

instance FromForm LoginForm

welcomeForm :: Text -> Maybe InviteID -> Html ()
welcomeForm pathPrefix inviteM = do
    loginForm pathPrefix "Welcome to ButlerOS" attrs
  where
    attrs = maybe [] (\invite -> ["invite" .= invite]) inviteM

newtype SessionTabs = SessionTabs (TVar (Map SessionID TabID))

newSessionTabs :: STM SessionTabs
newSessionTabs = SessionTabs <$> newTVar mempty

getTab :: SessionTabs -> SessionID -> STM TabID
getTab (SessionTabs vMap) sessionID = stateTVar vMap $ \m ->
    let tab = (+ 1) $ fromMaybe 0 $ Map.lookup sessionID m
     in (tab, Map.insert sessionID tab m)

authServer :: Sessions -> SessionTabs -> (Html () -> Html ()) -> JWTSettings -> ServerT AuthAPI ProcessIO
authServer sessions tabs mkIndexHtml jwtSettings auth =
    let loginSrv = loginServer sessions tabs mkIndexHtml jwtSettings auth
     in loginSrv Nothing :<|> (loginSrv . Just)

loginServer :: Sessions -> SessionTabs -> (Html () -> Html ()) -> JWTSettings -> AuthResult SessionID -> Maybe Workspace -> ServerT LoginAPI ProcessIO
loginServer sessions tabs mkIndexHtml jwtSettings auth workspaceM = indexRoute auth :<|> getSessionRoute
  where
    pathPrefix = case workspaceM of
        Nothing -> ""
        Just (Workspace ws) -> "/" <> ws

    indexRoute :: AuthResult SessionID -> Maybe InviteID -> ProcessIO (Html ())
    indexRoute ar inviteM = liftIO $ case ar of
        Authenticated sessionID -> do
            isSessionValid <- atomically (checkSession sessions sessionID)
            case isSessionValid of
                Just _ -> do
                    tabID <- atomically (getTab tabs sessionID)
                    pure $ mkIndexHtml (websocketHtml pathPrefix sessionID tabID)
                Nothing -> loginPage
        _OtherAuth -> loginPage
      where
        loginPage = do
            isValid <- atomically (validClient inviteM)
            pure $
                mkIndexHtml $
                    splashHtml $
                        if isValid
                            then welcomeForm pathPrefix inviteM
                            else div_ "access denied"

    swapSplash = with div_ [id_ "display-lock", hxSwapOob_ "outerHTML"]

    denyResp :: ProcessIO AuthResp
    denyResp = clearSession cookieSettings . swapSplash <$> indexRoute SAS.NoSuchUser Nothing

    validClient :: Maybe InviteID -> STM Bool
    validClient = \case
        Just invite -> checkInvite sessions invite
        Nothing -> isEmptySessions sessions

    welcomeResp :: SessionID -> ProcessIO AuthResp
    welcomeResp sessionID = do
        resp <- liftIO $ SAS.acceptLogin cookieSettings jwtSettings sessionID
        case resp of
            Just r -> do
                tabID <- atomically (getTab tabs sessionID)
                pure $ r (swapSplash $ websocketHtml pathPrefix sessionID tabID)
            Nothing -> error "oops?!"

    getSessionRoute :: LoginForm -> ProcessIO AuthResp
    getSessionRoute form = case isValidUserName (coerce form.username) of
        Just _username -> do
            logInfo "Validating form" ["form" .= form]
            sessionM <- createSession sessions form.username form.invite
            case sessionM of
                Just session -> welcomeResp session.sessionID
                Nothing -> denyResp
        Nothing -> denyResp

cookieSettings :: CookieSettings
cookieSettings =
    defaultCookieSettings
        { cookieSameSite = SameSiteStrict
        , cookieXsrfSetting = Nothing
        , cookieExpires = Just (UTCTime (fromGregorian 2030 1 1) 0)
        }

invitationAuthApp :: (Html () -> Html ()) -> Sessions -> ProcessIO AuthApplication
invitationAuthApp mkIndexHtml sessions = do
    JwkStorage myKey <- fst <$> newProcessMemory "display-key.jwk" (JwkStorage <$> liftIO generateKey)
    tabs <- atomically newSessionTabs
    let jwtSettings = defaultJWTSettings myKey

    let cfg = cookieSettings :. jwtSettings :. EmptyContext
    let authSrv = authServer sessions tabs mkIndexHtml jwtSettings
    env <- ask
    let app = Servant.serveWithContextT (Proxy @AuthAPI) cfg (liftIO . runProcessIO env.os env.process) authSrv
        getSession = \case
            Nothing -> pure Nothing
            Just sessionID -> atomically $ checkSession sessions sessionID
    pure $ AuthApplication app getSession
