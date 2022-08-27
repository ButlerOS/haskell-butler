module Butler.Network (WaiApplication, webService) where

import Network.HTTP.Types.Status qualified as HTTP
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Handler.WarpTLS qualified as Warp

import Butler.OS
import Butler.Prelude
import Butler.Storage

type WaiApplication = Wai.Application

getKeys :: ProcessIO (ByteString, ByteString)
getKeys = do
    os <- asks os
    Just k <- readStorage os.storage "key.pem"
    Just certificate <- readStorage os.storage "certificate.pem"
    pure (from certificate, from k)

webService :: [XStaticFile] -> Wai.Application -> Port -> ProcessIO Void
webService xs app port = do
    keys <- getKeys
    liftIO $ Warp.runTLS (uncurry Warp.tlsSettingsMemory keys) settings handler
    error "warp exited?!"
  where
    settings = Warp.setPort port Warp.defaultSettings
    staticApp = xstaticApp xs
    handler req resp =
        app
            req
            ( \appResp -> case HTTP.statusCode (Wai.responseStatus appResp) of
                404 -> staticApp req resp
                _ -> resp appResp
            )
