-- | Helpers to work with Servant
module Butler.Servant (
    -- * Servant type tetris
    AsProcessIO,
    ServantProcessIO,
    toServantHandler,

    -- * Auth helpers
    AuthContext (..),
    JwkStorage (..),
    newAuthContext,
    cookieSettings,

    -- * API
    AuthResp,
    BaseAPI,
    withBaseAPI,

    -- * Helpers
    throwRedirect,
) where

import Butler.Core
import Butler.Display.Session
import Butler.Display.WebSocket
import Butler.Prelude

import Codec.Serialise.Decoding (decodeBytes)
import Codec.Serialise.Encoding (encodeBytes)
import Control.Monad.Except (ExceptT (..))
import Crypto.JOSE.JWK qualified as JOSE
import Data.Aeson (decodeStrict')
import Data.Time (UTCTime (..), fromGregorian)
import Servant (Capture, Context ((:.)), NamedRoutes, (:-), (:>))
import Servant qualified
import Servant.Auth as SA
import Servant.Auth.Server qualified as SAS
import Servant.Server.Generic qualified

-- | A ProcessIO wrapper to enable performing servant redirection through 'throwError $ err303'
type ServantProcessIO = ExceptT Servant.ServerError ProcessIO

-- | Enables using ServantProcessIO as a NamedRoutes 'mode'
type AsProcessIO = Servant.Server.Generic.AsServerT ServantProcessIO

data AuthContext = AuthContext
    { servantContext :: Servant.Context '[SAS.CookieSettings, SAS.JWTSettings]
    , jwtSettings :: SAS.JWTSettings
    }

toServantHandler :: ProcessEnv -> ServantProcessIO a -> Servant.Handler a
toServantHandler env (ExceptT action) = Servant.Handler $ ExceptT $ runProcessIO env.os env.process action

mkAuthContext :: JwkStorage -> AuthContext
mkAuthContext (JwkStorage jwk) = AuthContext ctx jwtSettings
  where
    jwtSettings = SAS.defaultJWTSettings jwk
    ctx = cookieSettings :. jwtSettings :. Servant.EmptyContext

newAuthContext :: ProcessIO AuthContext
newAuthContext = do
    jwk <- fst <$> newProcessMemory "display-key.jwk" (JwkStorage <$> liftIO SAS.generateKey)
    pure $ mkAuthContext jwk

cookieSettings :: SAS.CookieSettings
cookieSettings =
    SAS.defaultCookieSettings
        { SAS.cookieSameSite = SAS.SameSiteStrict
        , SAS.cookieXsrfSetting = Nothing
        , SAS.cookieExpires = Just (UTCTime (fromGregorian 2030 1 1) 0)
        }

newtype JwkStorage = JwkStorage JOSE.JWK

instance Serialise JwkStorage where
    encode (JwkStorage jwk) = encodeBytes (from $ encodeJSON jwk)
    decode = fmap decodeJWK decodeBytes
      where
        decodeJWK :: ByteString -> JwkStorage
        decodeJWK bs = JwkStorage (fromMaybe (error "bad encoding?!") $ decodeStrict' bs)

-- | Root api
type AuthResp = Servant.Headers '[Servant.Header "Set-Cookie" SAS.SetCookie, Servant.Header "Set-Cookie" SAS.SetCookie] (Html ())

type BaseAPI api = NamedRoutes (AuthAPI (WorkspaceAPI api))

-- | Wrap the given NamedRoutes api with Auth and Workspace.
withBaseAPI :: (SAS.AuthResult SessionID -> Maybe Workspace -> api AsProcessIO) -> AuthAPI (WorkspaceAPI api) AsProcessIO
withBaseAPI cb = withAuthAPI (withWorkspaceAPI . cb)

data WorkspaceAPI api mode = WorkspaceAPI
    { withoutWorkspace :: mode :- NamedRoutes api
    , withWorkspace :: mode :- Capture "workspace" Workspace :> NamedRoutes api
    }
    deriving (Generic)

withWorkspaceAPI :: (Maybe Workspace -> api AsProcessIO) -> WorkspaceAPI api AsProcessIO
withWorkspaceAPI api =
    WorkspaceAPI
        { withoutWorkspace = api Nothing
        , withWorkspace = api . Just
        }

-- | Auth api
newtype AuthAPI api mode = AuthAPI
    { authRoute :: mode :- SAS.Auth '[SA.JWT, SA.Cookie] SessionID :> NamedRoutes api
    }
    deriving (Generic)

withAuthAPI :: (SAS.AuthResult SessionID -> api AsProcessIO) -> AuthAPI api AsProcessIO
withAuthAPI = AuthAPI

-- | Redirect the user.
throwRedirect :: Text -> ServantProcessIO a
throwRedirect path = throwError $ Servant.err303{Servant.errHeaders = [("Location", encodeUtf8 path)]}
