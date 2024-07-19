module Butler.Core.Network (
    WaiApplication,
    webService,
    WebProtocol (..),
    withUnixSocket,
    unixService,
    ServerName,
) where

import Data.ByteString qualified as BS
import Network.HTTP.Types.Status qualified as HTTP
import Network.Socket qualified as Socket
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Handler.WarpTLS qualified as Warp
import System.Process.Typed

import Butler.Core
import Butler.Prelude
import System.Directory (removeFile)
import UnliftIO.Directory (withCurrentDirectory)

type WaiApplication = Wai.Application

-- | Listening mode.
data WebProtocol
    = Http
    | -- Https (Maybe (crt data, key data)). On Nothing, 'webService' generates a self-signed certificate.
      Https (Maybe (ByteString, ByteString))
    deriving (Eq)

getKeys :: ProcessIO (ByteString, ByteString)
getKeys = fst <$> newProcessMemory "tls.key" genKeys
  where
    genKeys = withSystemTempDirectory "butler-gen-keys" $ \tmpDir -> withCurrentDirectory tmpDir do
        let keyPath = tmpDir </> "key.pem"
            reqPath = tmpDir </> "crt.csr"
            crtPath = tmpDir </> "crt.pem"
        runExternalProcess "openssl-genrsa" $ proc "openssl" ["genrsa", "-out", keyPath, "2048"]
        runExternalProcess "openssl-req" $ proc "openssl" ["req", "-new", "-key", keyPath, "-out", reqPath, "-subj", "/CN=localhost"]
        runExternalProcess "openssl-sign" $ proc "openssl" ["x509", "-req", "-in", reqPath, "-signkey", keyPath, "-out", crtPath]
        liftIO do
            crtData <- BS.readFile crtPath
            keyData <- BS.readFile keyPath
            pure (crtData, keyData)

withSocket :: (Socket -> ProcessIO a) -> ProcessIO a
withSocket =
    bracket (liftIO $ Socket.socket Socket.AF_UNIX Socket.Stream Socket.defaultProtocol) (liftIO . Socket.close)

withUnixSocket :: FilePath -> (Socket -> ProcessIO a) -> ProcessIO a
withUnixSocket fp cb = withSocket \socket -> do
    liftIO (Socket.connect socket (Socket.SockAddrUnix fp))
    cb socket

unixService :: FilePath -> (Socket -> ProcessIO ()) -> ProcessIO Void
unixService fp cb = withSocket \socket -> do
    cleanup
    bracket_ (liftIO $ Socket.bind socket (Socket.SockAddrUnix fp)) cleanup do
        liftIO $ Socket.listen socket 5
        forever do
            (client, _) <- liftIO $ Socket.accept socket
            cb client
  where
    cleanup = ignoringExceptions (liftIO $ removeFile fp)

-- | The fqdn:port the client connected to. Use this to render absolute url.
newtype ServerName = ServerName Text
    deriving newtype (IsString)

instance From ServerName Text where from (ServerName n) = n

webService :: [XStaticFile] -> (ServerName -> WaiApplication) -> Port -> WebProtocol -> ProcessIO Void
webService xs app port = \case
    Http -> do
        logInfo "Running WARP" ["port" .= port, "tls" .= False]
        liftIO $ Warp.runSettings settings handlerInsecure
        error "warp exited?!!"
    Https mKeys -> do
        keys <- maybe getKeys pure mKeys
        logInfo "Running WARP" ["port" .= port, "tls" .= True]
        liftIO $ Warp.runTLS (allowInsecure $ uncurry Warp.tlsSettingsMemory keys) settings handlerTLS
        error "warp exited?!"
  where
    allowInsecure tlsSettings = tlsSettings{Warp.onInsecure = Warp.AllowInsecure}
    settings = Warp.setPort port Warp.defaultSettings
    staticApp = xstaticApp xs

    handlerTLS, handlerInsecure :: WaiApplication
    handlerTLS req resp
        | Wai.isSecure req = handler serverName req resp
        | otherwise = resp $ Wai.responseLBS HTTP.status301 [("Location", secureLocation)] mempty
      where
        serverName = ServerName $ decodeUtf8 secureHost
        secureLocation = "https://" <> secureHost <> req.rawPathInfo <> req.rawQueryString
        secureHost :: ByteString
        secureHost = fromMaybe defaultHost req.requestHeaderHost
        defaultHost =
            "localhost" <> case port of
                443 -> ""
                _ -> ":" <> encodeUtf8 (showT port)

    handlerInsecure req resp = handler serverName req resp
      where
        serverName =
            ServerName $
                maybe "localhost" decodeUtf8 req.requestHeaderHost <> case port of
                    80 -> ""
                    _ -> ":" <> showT port

    handler serverName req resp =
        app
            serverName
            req
            ( \appResp -> case HTTP.statusCode (Wai.responseStatus appResp) of
                404 -> staticApp req resp
                _ -> resp appResp
            )
