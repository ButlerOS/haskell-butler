module Butler.Auth.Guest (guestAuthApp) where

import Lucid

import Butler.Core
import Butler.Display
import Butler.Display.GUI
import Butler.Display.Session
import Butler.Display.WebSocket
import Butler.Prelude
import Butler.Servant

import Servant
import Servant.Auth.Server as SAS
import Servant.HTML.Lucid

data LoginAPI mode = LoginAPI
    { indexRoute :: mode :- Get '[HTML] (Html ())
    , loginRoute :: mode :- "_login" :> Get '[HTML] AuthResp
    }
    deriving (Generic)

loginServer :: Sessions -> (Html () -> Html ()) -> JWTSettings -> AuthResult SessionID -> Maybe Workspace -> LoginAPI AsProcessIO
loginServer sessions mkIndexHtml jwtSettings auth mWorkspace = LoginAPI{indexRoute, loginRoute}
  where
    -- Create session on _login request
    loginRoute :: ServantProcessIO AuthResp
    loginRoute = lift do
        session <- newSession sessions Nothing "guest" Nothing
        liftIO (SAS.acceptLogin cookieSettings jwtSettings session.sessionID) >>= \case
            Just r -> do
                let page = workspaceUrl mWorkspace
                pure $ r (script_ $ "window.location.href = " <> showT page)
            Nothing -> error "oops?!"

    indexRoute :: ServantProcessIO (Html ())
    indexRoute = case auth of
        Authenticated sessionID -> lift do
            mSession <- atomically (lookupSession sessions sessionID)
            case mSession of
                Just _ ->
                    pure $ mkIndexHtml (websocketHtml (workspaceUrl mWorkspace))
                Nothing -> do
                    logError "no more auth?" ["id" .= sessionID]
                    pure "Unknown session"
        _ -> throwError $ err303{errHeaders = [("Location", encodeUtf8 (workspaceUrl mWorkspace) <> "_login")]}

guestAuthApp :: AuthContext -> Sessions -> (Html () -> Html ()) -> ProcessIO AuthApplication
guestAuthApp authContext sessions mkIndexHtml = do
    env <- ask
    let srv = loginServer sessions mkIndexHtml authContext.jwtSettings
    let app = Servant.serveWithContextT (Proxy @(BaseAPI LoginAPI)) authContext.servantContext (toServantHandler env) (withBaseAPI srv)
    pure $ AuthApplication app
