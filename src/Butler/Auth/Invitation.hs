module Butler.Auth.Invitation (invitationAuthApp) where

import Control.Monad.Except (runExceptT)
import Data.Time (UTCTime (..), fromGregorian)
import Lucid
import Servant
import Servant.Auth as SA
import Servant.Auth.Server as SAS
import Servant.HTML.Lucid
import Web.FormUrlEncoded (FromForm)

import Butler.Core
import Butler.Display
import Butler.Display.GUI
import Butler.Display.Session
import Butler.Display.WebSocket
import Butler.Prelude

type AuthResp = Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] (Html ())

type RootAPI = QueryParam "invite" InviteID :> Get '[HTML] (Html ())

type RecoveryAPI =
    "_recovery" :> QueryParam "recover" RecoveryID :> Get '[HTML] AuthResp

type LoginAPI =
    "_login" :> ReqBody '[FormUrlEncoded] LoginForm :> Post '[HTML] AuthResp

type InvitationAPI = RootAPI :<|> RecoveryAPI :<|> LoginAPI

type AuthAPI = Auth '[SA.JWT, SA.Cookie] SessionID :> (InvitationAPI :<|> Capture "workspace" Workspace :> InvitationAPI)

data LoginForm = LoginForm
    { invite :: Maybe InviteID
    , username :: UserName
    }
    deriving (Generic, FromJSON, ToJSON)

instance FromForm LoginForm

welcomeForm :: Maybe Workspace -> Maybe InviteID -> Html ()
welcomeForm mWorkspace mInvite =
    with form_ [id_ "login-form", action_ url, method_ "post"] do
        with (input_ mempty) [type_ "hidden", name_ "invite", value_ (maybe "" coerce mInvite)]
        with div_ [class_ "font-semibold pb-2 flex flex-row justify-center"] do
            "Welcome to ButlerOS"
        with (input_ mempty) [name_ "username", type_ "text", placeholder_ "What is your name?"]
  where
    url = workspaceUrl mWorkspace <> "_login"

authServer :: Sessions -> (Html () -> Html ()) -> JWTSettings -> ServerT AuthAPI ProcessIO
authServer sessions mkIndexHtml jwtSettings auth =
    let loginSrv = loginServer sessions jwtSettings
        rootSrv = rootServer sessions mkIndexHtml auth
        recoverySrv = recoveryServer sessions jwtSettings
        mkAPI mWorkspace = rootSrv mWorkspace :<|> recoverySrv mWorkspace :<|> loginSrv mWorkspace
     in mkAPI Nothing :<|> mkAPI . Just

redirect :: Monad m => Maybe Workspace -> HtmlT m ()
redirect mWorkspace = script_ $ "window.location.href = " <> showT (workspaceUrl mWorkspace)

rootServer :: Sessions -> (Html () -> Html ()) -> AuthResult SessionID -> Maybe Workspace -> ServerT RootAPI ProcessIO
rootServer sessions mkIndexHtml ar mWorkspace = indexRoute
  where
    indexRoute :: Maybe InviteID -> ProcessIO (Html ())
    indexRoute mInvite = liftIO $ case ar of
        Authenticated sessionID -> do
            mSession <- atomically (lookupSession sessions sessionID)
            case mSession of
                Just _ -> pure $ mkIndexHtml (websocketHtml (workspaceUrl mWorkspace) sessionID)
                Nothing -> loginPage
        _OtherAuth -> loginPage
      where
        loginPage = do
            isValid <- atomically (validClient mInvite)
            pure . mkIndexHtml . with div_ [id_ "display-ws"] . splashHtml $
                if isValid
                    then welcomeForm mWorkspace mInvite
                    else div_ "access denied"

    validClient :: Maybe InviteID -> STM Bool
    validClient = \case
        Just invite -> checkInvite sessions invite
        Nothing -> isEmptySessions sessions

recoveryServer :: Sessions -> JWTSettings -> Maybe Workspace -> ServerT RecoveryAPI ProcessIO
recoveryServer sessions jwtSettings mWorkspace mRecover = case mRecover of
    Nothing -> denyResp
    Just recoveryID -> do
        atomically (getSessionFromRecover sessions recoveryID) >>= \case
            Nothing -> denyResp
            Just session -> do
                resp <- liftIO $ SAS.acceptLogin cookieSettings jwtSettings session.sessionID
                case resp of
                    Just r -> pure $ r $ redirect mWorkspace
                    Nothing -> error "oops!"
  where
    denyResp :: ProcessIO AuthResp
    denyResp = pure $ clearSession cookieSettings "Invalid recovery link :/"

loginServer :: Sessions -> JWTSettings -> Maybe Workspace -> ServerT LoginAPI ProcessIO
loginServer sessions jwtSettings mWorkspace = getSessionRoute
  where
    denyResp :: ProcessIO AuthResp
    denyResp = pure $ clearSession cookieSettings "Invalid invite or username :/"

    welcomeResp :: SessionID -> ProcessIO AuthResp
    welcomeResp sessionID = do
        resp <- liftIO $ SAS.acceptLogin cookieSettings jwtSettings sessionID
        case resp of
            Just r -> pure $ r $ redirect mWorkspace
            Nothing -> error "oops?!"

    getSessionRoute :: LoginForm -> ProcessIO AuthResp
    getSessionRoute form = do
        res <- withMVar sessions.lock \() -> runExceptT @Text do
            lift $ logInfo "Validating form" ["form" .= form]
            invite <- case form.invite of
                Just invite -> pure invite
                Nothing -> throwError "No invite"
            unlessM (atomically (checkInvite sessions invite)) do
                throwError "Bad invite"
            username <- case isValidUserName (coerce form.username) of
                Just username -> pure username
                Nothing -> throwError "Bad username"
            unlessM (lift $ isUsernameAvailable sessions form.username) do
                throwError "Username already taken"
            lift $ newSession sessions username

        case res of
            Left err -> do
                logError "access denied" ["err" .= err]
                denyResp
            Right session -> welcomeResp session.sessionID

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
            Just sessionID -> atomically $ lookupSession sessions sessionID
    pure $ AuthApplication app getSession
