module Butler.App.QRTest where

import Butler

import Codec.QRCode qualified as QRCode
import Codec.QRCode.JuicyPixels qualified as QRCode

qrApp :: App
qrApp =
    (defaultApp "qr-test" startQrTest)
        { tags = fromList ["Utility"]
        , description = "Create qr-code"
        }

qrEncode :: Text -> HtmlT STM ()
qrEncode value = case QRCode.encodeText (QRCode.defaultQRCodeOptions QRCode.H) QRCode.Utf8WithECI value of
    Nothing -> "qr encode failed"
    Just qrImage -> img_ [src_ (from $ QRCode.toPngDataUrlT 4 4 qrImage)]

startQrTest :: AppContext -> ProcessIO ()
startQrTest ctx = do
    state <- newTVarIO Nothing
    let mountUI = with div_ [wid_ ctx.wid "w"] do
            with div_ [class_ "flex flex-col items-center justify-center gap-1 m-2"] do
                h1_ "QR Test"
                with form_ [wid_ ctx.wid "render", wsSend, hxTrigger_ "submit"] do
                    with (input_ mempty) [class_ inputClass, name_ "title", type_ "text", placeholder_ "Content..."]
                lift (readTVar state) >>= \case
                    Nothing -> pure ()
                    Just h -> h

    forever do
        atomically (readPipe ctx.pipe) >>= \case
            ae@AppDisplay{} -> sendHtmlOnConnect mountUI ae
            AppTrigger ge -> case ge.body ^? key "title" . _JSON of
                Just title -> do
                    atomically $ writeTVar state (Just $ qrEncode title)
                    sendsHtml ctx.shared.clients mountUI
                Nothing -> logError "unknown ev" ["ev" .= ge]
            _ -> pure ()
