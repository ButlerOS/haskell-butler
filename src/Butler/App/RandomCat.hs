module Butler.App.RandomCat (randomCatApp) where

import Butler
import Network.HTTP.Client (
    Manager,
    Response (responseBody, responseStatus),
    brConsume,
    parseRequest_,
    withResponse,
 )
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types (statusIsSuccessful)

import Data.ByteString.Base64 qualified as B64 (encode)
import Data.ByteString.Char8 qualified as C8

randomCatApp :: App
randomCatApp =
    (defaultApp "RandomCat" startRandomCat)
        { tags = fromList ["Utility"]
        , description = "Display a random cat using CATAAS"
        , size = Just (500, 500)
        }

data RandomCatState = Loading | LoadingError | Loaded ByteString

startRandomCat :: AppContext -> ProcessIO ()
startRandomCat ctx = do
    -- Setup state
    logInfo "RandomCat started!" []
    state <- newTVarIO Nothing
    let setState = atomically . writeTVar state . Just

    manager <- newTlsManager

    -- UI
    let mountUI :: HtmlT STM ()
        mountUI = with div_ [wid_ ctx.wid "w", class_ "flex flex-col gap-2"] do
            withTrigger_ "click" ctx.wid "get-a-cat" button_ [] "Give me a cat !"
            div_ [wid_ ctx.wid "randomCat", class_ "flex justify-center"] $ do
                lift (readTVar state) >>= \case
                    Just Loading -> p_ "loading ..."
                    Just (Loaded catBS) -> do
                        let imgSrc = "data:image/jpeg;base64," <> C8.unpack (B64.encode catBS)
                        img_ [src_ $ from imgSrc]
                    Just LoadingError -> p_ "loading error !"
                    Nothing -> pure ()

    -- Handle events
    forever do
        atomically (readPipe ctx.pipe) >>= \case
            AppDisplay (UserJoined client) -> atomically $ sendHtml client mountUI
            AppTrigger ev -> do
                case ev.trigger of
                    "get-a-cat" -> do
                        setState Loading
                        sendsHtml ctx.shared.clients mountUI
                        catM <- getACat manager
                        case catM of
                            Just cat -> setState $ Loaded cat
                            Nothing -> setState LoadingError
                    _ -> logError "Unknown trigger" ["ev" .= ev]
                sendsHtml ctx.shared.clients mountUI
            ev -> logError "Unknown ev" ["ev" .= ev]

getACat :: Manager -> ProcessIO (Maybe ByteString)
getACat manager = performRequest getCatRequest
  where
    getCatRequest = parseRequest_ "https://cataas.com/cat?width=400&height=400"
    performRequest req = do
        liftIO $ withResponse req manager $ \r -> do
            if statusIsSuccessful r.responseStatus
                then do
                    content <- brConsume $ r.responseBody
                    pure $ Just $ mconcat content
                else pure Nothing