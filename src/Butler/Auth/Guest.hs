module Butler.Auth.Guest (guestAuthApp) where

import Lucid

import Butler.Core
import Butler.Display
import Butler.Display.GUI
import Butler.Display.Session
import Butler.Display.WebSocket
import Butler.Prelude

import Data.Time (UTCTime (..), fromGregorian)
import Servant
import Servant.Auth as SA
import Servant.Auth.Server as SAS
import Servant.HTML.Lucid

type AuthResp = Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] (Html ())
type LoginAPI = Get '[HTML] (Html ()) :<|> "_login" :> Get '[HTML] AuthResp

type AuthAPI = Auth '[SA.JWT, SA.Cookie] SessionID :> (LoginAPI :<|> Capture "workspace" Workspace :> LoginAPI)

authServer :: OS -> Process -> Sessions -> (Html () -> Html ()) -> JWTSettings -> ServerT AuthAPI Servant.Handler
authServer os process sessions mkIndexHtml jwtSettings auth =
    let loginSrv = loginServer os process sessions mkIndexHtml jwtSettings auth
     in loginSrv Nothing :<|> (loginSrv . Just)

loginServer :: OS -> Process -> Sessions -> (Html () -> Html ()) -> JWTSettings -> AuthResult SessionID -> Maybe Workspace -> ServerT LoginAPI Servant.Handler
loginServer os process sessions mkIndexHtml jwtSettings auth mWorkspace = indexRoute auth :<|> getSessionRoute
  where
    -- Create session on _login request
    getSessionRoute :: Servant.Handler AuthResp
    getSessionRoute = do
        session <- liftIO $ runProcessIO os process $ newSession sessions "guest"
        liftIO (SAS.acceptLogin cookieSettings jwtSettings session.sessionID) >>= \case
            Just r -> do
                let page = workspaceUrl mWorkspace
                pure $ r (script_ $ "window.location.href = " <> showT page)
            Nothing -> error "oops?!"

    indexRoute :: AuthResult SessionID -> Servant.Handler (Html ())
    indexRoute = \case
        Authenticated sessionID -> do
            mSession <- atomically (lookupSession sessions sessionID)
            case mSession of
                Just _ ->
                    pure $ mkIndexHtml (websocketHtml (workspaceUrl mWorkspace) sessionID)
                Nothing -> do
                    liftIO $ runProcessIO os process $ logError "no more auth?" ["id" .= sessionID]
                    pure "Unknown session"
        _ -> throwError $ err303{errHeaders = [("Location", encodeUtf8 (workspaceUrl mWorkspace) <> "_login")]}

cookieSettings :: CookieSettings
cookieSettings =
    defaultCookieSettings
        { cookieSameSite = SameSiteStrict
        , cookieXsrfSetting = Nothing
        , cookieExpires = Just (UTCTime (fromGregorian 2030 1 1) 0)
        }

guestAuthApp :: Sessions -> (Html () -> Html ()) -> ProcessIO AuthApplication
guestAuthApp sessions mkIndexHtml = do
    JwkStorage myKey <- fst <$> newProcessMemory "display-key.jwk" (JwkStorage <$> liftIO generateKey)
    let jwtSettings = defaultJWTSettings myKey
    let cfg = cookieSettings :. jwtSettings :. EmptyContext
    env <- ask
    let authSrv = authServer env.os env.process sessions mkIndexHtml jwtSettings
    let app = Servant.serveWithContextT (Proxy @AuthAPI) cfg id authSrv
    pure $ AuthApplication app
