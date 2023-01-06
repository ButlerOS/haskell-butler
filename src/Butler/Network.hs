module Butler.Network (
    WaiApplication,
    webService,
    WebProtocol (..),
) where

import Data.ByteString qualified as BS
import Network.HTTP.Types.Status qualified as HTTP
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Handler.WarpTLS qualified as Warp
import System.Process.Typed

import Butler.OS
import Butler.Prelude
import System.FilePath ((</>))
import UnliftIO.Directory (withCurrentDirectory)

type WaiApplication = Wai.Application

data WebProtocol
    = Http
    | Https (Maybe (ByteString, ByteString))

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

webService :: [XStaticFile] -> Wai.Application -> Port -> WebProtocol -> ProcessIO Void
webService xs app port = \case
    Http -> error "TODO"
    Https mKeys -> do
        keys <- maybe getKeys pure mKeys
        logInfo "Running WARP" ["port" .= port, "tls" .= True]
        liftIO $ Warp.runTLS (allowInsecure $ uncurry Warp.tlsSettingsMemory keys) settings handlerTLS
        error "warp exited?!"
  where
    allowInsecure tlsSettings = tlsSettings{Warp.onInsecure = Warp.AllowInsecure}
    settings = Warp.setPort port Warp.defaultSettings
    staticApp = xstaticApp xs
    handlerTLS req resp
        | Wai.isSecure req = handler req resp
        | otherwise = resp $ Wai.responseLBS HTTP.status301 [("Location", secureLocation)] mempty
      where
        secureLocation = "https://" <> secureHost <> req.rawPathInfo <> req.rawQueryString
        secureHost = fromMaybe defaultHost req.requestHeaderHost
        defaultHost = "localhost:" <> encodeUtf8 (from $ show port)
    handler req resp =
        app
            req
            ( \appResp -> case HTTP.statusCode (Wai.responseStatus appResp) of
                404 -> staticApp req resp
                _ -> resp appResp
            )
