module Butler.WebSocket (
    WebSocketAPI,
    WebSocketServer (..),
    websocketServer,
    splashHtml,
    ChannelName (..),
    Workspace (..),
) where

import Lucid
import Lucid.Base (makeAttribute)
import Network.Socket
import Network.WebSockets qualified as WS
import Servant
import Servant.API.WebSocket
import Servant.HTML.Lucid

import Lucid.Htmx
import Servant.Auth as SA
import Servant.Auth.Server as SAS

import Butler.OS
import Butler.Prelude
import Butler.Session
import Web.FormUrlEncoded (FromForm)

newtype Workspace = Workspace Text
    deriving newtype (Eq, Ord, Show, IsString, FromHttpApiData)
    deriving (FromJSON, ToJSON, ToHtml) via Text

instance From Workspace Text where
    from (Workspace n) = n

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

websocketHtml :: Text -> SessionID -> Html ()
websocketHtml pathPrefix sessionID = do
    let wsUrl = pathPrefix <> "/ws/htmx?session=" <> from sessionID
    with div_ [id_ "display-ws", class_ "h-full", makeAttribute "hx-ext" "ws", makeAttribute "ws-connect" wsUrl] do
        with div_ [id_ "display-root", class_ "h-full"] mempty
        script_ $ "globalThis.wsUrl = n => 'wss://' + window.location.host + '" <> pathPrefix <> "/ws/' + n + '?session=" <> from sessionID <> "';"

splashHtml :: Monad m => HtmlT m () -> HtmlT m ()
splashHtml content = do
    with div_ [id_ "display-lock", class_ "h-screen w-screen absolute bg-gray-100 flex flex-col"] do
        with div_ [class_ "basis-1/6 flex bg-sky-600 border-sky-800 border-b-8"] mempty

        with div_ [class_ "grow flex bg-sky-200 flex-col justify-center"] do
            with div_ [class_ "flex flex-row justify-center"] do
                with div_ [class_ "p-3 rounded"] do
                    content

        with div_ [class_ "basis-1/6 flex bg-sky-600 border-sky-800 border-t-8"] mempty

welcomeForm :: Text -> Maybe InviteID -> Html ()
welcomeForm pathPrefix inviteM = do
    with form_ [id_ "splash-form", hxPost_ (pathPrefix <> "/login")] do
        with div_ [class_ "font-semibold pb-2 flex flex-row justify-center"] do
            "Welcome to ButlerOS"
        with (input_ mempty) [name_ "username", type_ "text", placeholder_ "What is your name?"]
        case inviteM of
            Just (InviteID invite) -> with (input_ mempty) [name_ "invite", type_ "hidden", value_ (from $ show invite)]
            _ -> pure ()

cookieSettings :: CookieSettings
cookieSettings = defaultCookieSettings{cookieIsSecure = NotSecure, cookieSameSite = SameSiteStrict, cookieXsrfSetting = Nothing}

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

type ClientWSAPI = "ws" :> Capture "channel" ChannelName :> QueryParam "reconnect" Bool :> QueryParam "session" SessionID :> WebSocket

type ClientAPI = ClientWSAPI :<|> (Capture "workspace" Workspace :> ClientWSAPI)

clientServer :: Sessions -> (Workspace -> ChannelName -> Session -> WS.Connection -> ProcessIO ()) -> ServerT ClientAPI ProcessIO
clientServer sessions onConnect = connectRoute Nothing :<|> connectRoute . Just
  where
    connectRoute :: Maybe Workspace -> ChannelName -> Maybe Bool -> Maybe SessionID -> WS.Connection -> ProcessIO ()
    connectRoute workspaceM name (fromMaybe False -> reconnect) sessionIDM connection
        | reconnect = doReload
        | otherwise = do
            isSessionValid <- atomically do
                case sessionIDM of
                    Just sessionID -> checkSession sessions sessionID
                    Nothing -> pure Nothing
            case isSessionValid of
                Nothing -> doReload
                Just session -> onConnect workspace name session connection
      where
        workspace = case workspaceM of
            Just ws -> ws
            Nothing -> Workspace ""
        doReload = liftIO $ WS.sendTextData connection $ renderText do
            with span_ [id_ "display-ws"] do
                "<reconnecting...>"
                script_ "window.location.reload()"

newtype ChannelName = ChannelName Text
    deriving newtype (Eq, Show, Ord, FromHttpApiData, ToJSON, IsString)

type WebSocketAPI = RemoteHost :> (AuthAPI :<|> ClientAPI)

data WebSocketServer = WebSocketServer
    { mkIndexHtml :: Html () -> Html ()
    , jwtSettings :: JWTSettings
    , onConnect :: SockAddr -> Workspace -> ChannelName -> Session -> WS.Connection -> ProcessIO ()
    }

websocketServer :: Sessions -> WebSocketServer -> ServerT WebSocketAPI ProcessIO
websocketServer sessions wss = displayRoute
  where
    displayRoute :: ServerT WebSocketAPI ProcessIO
    displayRoute clientAddr =
        authServer sessions wss.mkIndexHtml wss.jwtSettings
            :<|> clientServer sessions (wss.onConnect clientAddr)
