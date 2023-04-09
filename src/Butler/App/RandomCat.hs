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

import Butler.App (withEvent)
import Data.ByteString.Base64 qualified as B64 (encode)
import Data.ByteString.Char8 qualified as C8

randomCatApp :: App
randomCatApp =
    (defaultApp "RandomCat" startRandomCat)
        { tags = fromList ["Game"]
        , description = "Display a random cat using CATAAS"
        , size = Just (500, 500)
        }

data RandomCatState = NoCat | LoadingCat | LoadingCatError | Cat ByteString

startRandomCat :: AppContext -> ProcessIO ()
startRandomCat ctx = do
    -- Setup state
    logInfo "RandomCat started!" []
    state <- newTVarIO NoCat
    let setState = atomically . writeTVar state

    manager <- newTlsManager

    -- UI
    let mountUI :: HtmlT STM ()
        mountUI = with div_ [wid_ ctx.wid "w", class_ "flex flex-col gap-2"] do
            let getACatButton = button_ [class_ "border-2 border-indigo-300 bg-indigo-100 p-1 m-1"] "Give me a 🐱 !"
            div_ [class_ "flex justify-center"] $ do
                withEvent ctx.wid "get-a-cat" [] getACatButton
            div_ [class_ "flex justify-center"] $ do
                lift (readTVar state) >>= \case
                    LoadingCat -> p_ "Maouhhh ..."
                    Cat catBS -> do
                        let imgSrc = "data:image/jpeg;base64," <> C8.unpack (B64.encode catBS)
                        img_ [src_ $ from imgSrc]
                    LoadingCatError -> p_ "Oh no - unable to get a Cat !"
                    NoCat -> pure ()

    -- Handle events
    forever do
        atomically (readPipe ctx.pipe) >>= \case
            AppDisplay (UserJoined client) -> atomically $ sendHtml client mountUI
            AppTrigger ev -> do
                case ev.trigger of
                    "get-a-cat" -> do
                        setState LoadingCat
                        sendsHtml ctx.shared.clients mountUI
                        catM <- getACat manager
                        case catM of
                            Just cat -> setState $ Cat cat
                            Nothing -> setState LoadingCatError
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
