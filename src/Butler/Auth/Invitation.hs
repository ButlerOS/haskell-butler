module Butler.Auth.Invitation (invitationAuthApp) where

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

authServer :: Sessions -> (Html () -> Html ()) -> JWTSettings -> ServerT AuthAPI ProcessIO
authServer sessions mkIndexHtml jwtSettings auth =
    let loginSrv = loginServer sessions mkIndexHtml jwtSettings auth
     in loginSrv Nothing :<|> (loginSrv . Just)

loginServer :: Sessions -> (Html () -> Html ()) -> JWTSettings -> AuthResult SessionID -> Maybe Workspace -> ServerT LoginAPI ProcessIO
loginServer sessions mkIndexHtml jwtSettings auth workspaceM = indexRoute auth :<|> getSessionRoute
  where
    pathPrefix = case workspaceM of
        Nothing -> ""
        Just (Workspace ws) -> "/" <> ws

    indexRoute :: AuthResult SessionID -> Maybe InviteID -> ProcessIO (Html ())
    indexRoute ar inviteM = liftIO $ case ar of
        Authenticated sessionID -> do
            isSessionValid <- atomically (checkSession sessions sessionID)
            case isSessionValid of
                Just _ -> pure $ mkIndexHtml (websocketHtml pathPrefix sessionID)
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
        pure $ case resp of
            Just r -> do
                r (swapSplash $ websocketHtml pathPrefix sessionID)
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
    let jwtSettings = defaultJWTSettings myKey

    let cfg = cookieSettings :. jwtSettings :. EmptyContext
    let authSrv = authServer sessions mkIndexHtml jwtSettings
    env <- ask
    let app = Servant.serveWithContextT (Proxy @AuthAPI) cfg (liftIO . runProcessIO env.os env.process) authSrv
        getSession = \case
            Nothing -> pure Nothing
            Just sessionID -> atomically $ checkSession sessions sessionID
    pure $ AuthApplication app getSession
