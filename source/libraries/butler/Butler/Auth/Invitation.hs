module Butler.Auth.Invitation (invitationAuthApp) where

import Lucid
import Servant
import Servant.Auth.Server as SAS
import Servant.HTML.Lucid
import Web.FormUrlEncoded (FromForm)

import Butler.Core
import Butler.Display
import Butler.Display.GUI
import Butler.Display.Session
import Butler.Display.WebSocket
import Butler.Prelude
import Butler.Servant

data LoginAPI mode = LoginAPI
    { indexRoute :: mode :- QueryParam "invite" InviteID :> Get '[HTML] (Html ())
    , recoveryRoute :: mode :- "_recovery" :> QueryParam "recover" RecoveryID :> Get '[HTML] AuthResp
    , loginRoute :: mode :- "_login" :> ReqBody '[FormUrlEncoded] LoginForm :> Post '[HTML] AuthResp
    }
    deriving (Generic)

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

redirect :: Monad m => Maybe Workspace -> HtmlT m ()
redirect mWorkspace = script_ $ "window.location.href = " <> showT (workspaceUrl mWorkspace)

loginServer :: Sessions -> (Html () -> Html ()) -> JWTSettings -> AuthResult SessionID -> Maybe Workspace -> LoginAPI AsProcessIO
loginServer sessions mkIndexHtml jwtSettings auth mWorkspace = LoginAPI{indexRoute, recoveryRoute, loginRoute}
  where
    indexRoute :: Maybe InviteID -> ServantProcessIO (Html ())
    indexRoute mInvite = liftIO $ case auth of
        Authenticated sessionID -> do
            mSession <- atomically (lookupSession sessions sessionID)
            case mSession of
                Just _ -> pure $ mkIndexHtml (websocketHtml (workspaceUrl mWorkspace))
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
        Just invite | invite /= "" -> checkInvite sessions invite
        _ -> isEmptySessions sessions

    recoveryRoute :: Maybe RecoveryID -> ServantProcessIO AuthResp
    recoveryRoute mRecover = lift $ case mRecover of
        Nothing -> denyResp
        Just recoveryID -> do
            atomically (getSessionFromRecover sessions recoveryID) >>= \case
                Nothing -> denyResp
                Just session -> do
                    resp <- liftIO $ SAS.acceptLogin cookieSettings jwtSettings session.sessionID
                    case resp of
                        Just r -> pure $ r $ redirect mWorkspace
                        Nothing -> error "oops!"

    denyResp :: ProcessIO AuthResp
    denyResp = pure $ clearSession cookieSettings "Invalid recovery link :/"

    welcomeResp :: SessionID -> ProcessIO AuthResp
    welcomeResp sessionID = do
        resp <- liftIO $ SAS.acceptLogin cookieSettings jwtSettings sessionID
        case resp of
            Just r -> pure $ r $ redirect mWorkspace
            Nothing -> error "oops?!"

    loginRoute :: LoginForm -> ServantProcessIO AuthResp
    loginRoute form = lift do
        res <- withMVar sessions.lock \() -> runExceptT @Text $ do
            lift $ logInfo "Validating form" ["form" .= form]
            case form.invite of
                Just invite | invite /= "" -> do
                    unlessM (atomically (checkInvite sessions invite)) do
                        throwError "Bad invite"
                _ -> do
                    unlessM (atomically (isEmptySessions sessions)) do
                        throwError "First user already created, invite is required"
            username <- case isValidUserName (coerce form.username) of
                Just username -> pure username
                Nothing -> throwError "Bad username"
            lift $ newSession sessions Nothing username Nothing

        case res of
            Left err -> do
                logError "access denied" ["err" .= err]
                denyResp
            Right session -> welcomeResp session.sessionID

invitationAuthApp :: (Html () -> Html ()) -> AuthContext -> Sessions -> ProcessIO AuthApplication
invitationAuthApp mkIndexHtml authContext sessions = do
    env <- ask
    let srv = loginServer sessions mkIndexHtml authContext.jwtSettings
    let app = Servant.serveWithContextT (Proxy @(BaseAPI LoginAPI)) authContext.servantContext (toServantHandler env) (withBaseAPI srv)
    pure $ AuthApplication app
