module Butler.App.RandomCat (randomCatApp) where

import Butler
import Network.HTTP.Client (
    HttpException,
    Manager,
    Request,
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

data ImageType = PNG | GIF

data RandomCatState
    = NoCat
    | LoadingCat
    | LoadingCatError Text
    | Cat ImageType ByteString

catButton :: Text -> HtmlT STM ()
catButton label =
    button_ [class_ "border-2 border-indigo-300 bg-indigo-100 p-1 m-1"] $
        toHtml label

startRandomCat :: AppContext -> ProcessIO ()
startRandomCat ctx = do
    -- Setup state
    logInfo "RandomCat started!" []
    state <- newTVarIO NoCat
    manager <- newTlsManager
    let setState = atomically . writeTVar state
        encodeImg img = C8.unpack $ B64.encode img
        renderImg iType img =
            let header = case iType of
                    PNG -> "data:image/jpeg;base64,"
                    GIF -> "data:image/git;base64,"
                imgSrc = header <> encodeImg img
             in img_ [src_ $ from imgSrc]

    -- UI
    let mountUI :: HtmlT STM ()
        mountUI = with div_ [wid_ ctx.wid "w", class_ "flex flex-col gap-2"] do
            div_ [class_ "flex flex-row justify-center"] $ do
                div_ [class_ "flex justify-center"] $ do
                    withEvent ctx.wid "get-a-cat" [] $ catButton "Give me a 🐱 !"
                div_ [class_ "flex justify-center"] $ do
                    withEvent ctx.wid "get-a-gif-cat" [] $ catButton "Give me a 🐱 Gif !!!"
            div_ [class_ "flex justify-center"] $ do
                lift (readTVar state) >>= \case
                    LoadingCat -> p_ "Maouhhh ..."
                    Cat iType img -> renderImg iType img
                    LoadingCatError err -> do
                        div_ [class_ "flex flex-col"] $ do
                            div_ "Oh no - unable to get a Cat !"
                            div_ $ toHtml err
                    NoCat -> pure ()

    let displayCat iType = do
            setState LoadingCat
            sendsHtml ctx.shared.clients mountUI
            catM <- getACat manager iType
            case catM of
                Right cat -> setState $ Cat iType cat
                Left err -> setState $ LoadingCatError $ from err

    -- Handle events
    forever do
        atomically (readPipe ctx.pipe) >>= \case
            AppDisplay (UserJoined client) -> atomically $ sendHtml client mountUI
            AppTrigger ev -> do
                case ev.trigger of
                    "get-a-cat" -> displayCat PNG
                    "get-a-gif-cat" -> displayCat GIF
                    _ -> logError "Unknown trigger" ["ev" .= ev]
                sendsHtml ctx.shared.clients mountUI
            ev -> logError "Unknown ev" ["ev" .= ev]

catRequest :: Request
catRequest = parseRequest_ "https://cataas.com/cat?width=400&height=400"

catGifRequest :: Request
catGifRequest = parseRequest_ "https://cataas.com/cat/gif?width=400&height=400"

getACat :: Manager -> ImageType -> ProcessIO (Either String ByteString)
getACat manager iType = performRequest $ case iType of
    PNG -> catRequest
    GIF -> catGifRequest
  where
    performRequest req = do
        er <- liftIO $ try $ withResponse req manager $ \r -> do
            if statusIsSuccessful r.responseStatus
                then do
                    content <- brConsume $ r.responseBody
                    pure $ Just $ mconcat content
                else pure Nothing
        case er of
            Left (exc :: HttpException) -> pure $ Left $ show exc
            Right Nothing -> pure $ Left "Unable to access cataas"
            Right (Just mCat) -> pure $ Right mCat
