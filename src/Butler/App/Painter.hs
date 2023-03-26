module Butler.App.Painter (painterApp) where

import Butler

painterApp :: App
painterApp =
    (defaultApp "painter" startPainterApp)
        { tags = fromList ["Utility", "Graphic"]
        , description = "Paint!"
        , start = startPainterApp
        }

startPainterApp :: AppContext -> ProcessIO ()
startPainterApp ctx = do
    let mountUI = do
            with div_ [wid_ ctx.wid "w"] do
                with canvas_ [wid_ ctx.wid "img"] mempty
                script_ (painterClient ctx.wid)
    forever do
        atomically (readPipe ctx.pipe) >>= \case
            ae@AppDisplay{} -> sendHtmlOnConnect mountUI ae
            _ -> pure ()

painterClient :: AppID -> Text
painterClient wid =
    [raw|
function setupPainterClient(Wid) {

}
|]
        <> ("\nsetupPainterClient(" <> showT wid <> ");")
