module Butler.App.PokerPlanner (pokerPlannerApp) where

import Butler
import Butler.Display.Session
import Butler.Display.User
import Data.Aeson (Value (Number))
import Data.Function (on)
import Data.List (minimumBy)
import Data.Map.Strict qualified as Map
import Data.Scientific (fromFloatDigits, toRealFloat)

pokerPlannerApp :: App
pokerPlannerApp =
    (defaultApp "poker-planner" startPokerPlannerApp)
        { tags = fromList ["Development"]
        , description = "Planning tool"
        , start = startPokerPlannerApp
        }

data PokerPlannerStatus = WaitingForGame | Playing Text | Finished Text

data VoteValue = Vote Float | Unknown deriving (Show)

validVotes :: [Float]
validVotes = [0, 0.5, 1, 2, 3, 5, 8, 13, 20, 40, 100]

voteValues :: [VoteValue] -> [Float]
voteValues = concatMap getValue
  where
    getValue = \case
        Vote n -> [n]
        Unknown -> []

{- | Vote suggestion

>>> voteSuggestion [Vote 0, Vote 3, Unknown, Vote 9]
Vote 5.0

>>> voteSuggestion []
Unknown

>>> voteSuggestion [Unknown]
Unknown

>>> voteSuggestion [Vote 3, Vote 3, Vote 1]
Vote 2.0

>>> voteSuggestion [Vote 3, Vote 1]
Vote 2.0

>>> voteSuggestion [Vote 20, Vote 5]
Vote 13.0

>>> voteSuggestion [Vote 5, Vote 20]
Vote 13.0
-}
voteSuggestion :: [VoteValue] -> VoteValue
voteSuggestion xs =
    let values :: [Float]
        values = voteValues xs
        average :: Float
        average = sum values / fromIntegral (length values)
        distances :: [(Float, Float)]
        distances = map (\value -> (abs (average - value), value)) (reverse validVotes)
     in case values of
            [] -> Unknown
            _ -> Vote $ snd (minimumBy (compare `on` fst) distances)

instance ToHtml VoteValue where
    toHtml = \case
        Vote 0.5 -> "1/2"
        Vote n -> toHtml (showT @Int (round n))
        Unknown -> "?"

instance ToJSON VoteValue where
    toJSON = \case
        Vote n -> Number (fromFloatDigits n)
        Unknown -> Null

instance FromJSON VoteValue where
    parseJSON = \case
        Number n -> pure $ Vote (toRealFloat n)
        Null -> pure Unknown
        _ -> fail "Unknown vote value"

data PlayerStatus = Thinking | Voted VoteValue

data Player = Player
    { client :: DisplayClient
    , status :: PlayerStatus
    }

data PokerPlannerState = PokerPlannerState
    { players :: TVar (Map Endpoint Player)
    , status :: TVar PokerPlannerStatus
    }

getVotes :: Map Endpoint Player -> [VoteValue]
getVotes = Map.foldr doGetVote []
  where
    doGetVote :: Player -> [VoteValue] -> [VoteValue]
    doGetVote p acc = case p.status of
        Thinking -> acc
        Voted v -> v : acc

startPokerPlannerApp :: AppContext -> ProcessIO ()
startPokerPlannerApp ctx = do
    let story :: Maybe Text
        story = do
            argv <- ctx.argv
            argv ^? key "story" . _JSON

    let initialState = maybe WaitingForGame Playing story

    state <- atomically do
        PokerPlannerState <$> newTVar mempty <*> newTVar initialState
    let waitingUI = do
            withTrigger_ "" ctx.wid "new-game" (input_ []) [type_ "text", placeholder_ "New game name...", name_ "name"]

    let renderPlayers :: (Endpoint -> PlayerStatus -> HtmlT STM ()) -> HtmlT STM ()
        renderPlayers renderStatus = do
            players <- Map.elems <$> lift (readTVar state.players)
            forM_ players \player -> do
                username <- lift (readTVar player.client.session.username)
                div_ do
                    userIcon username
                    toHtml username
                    renderStatus player.client.endpoint player.status

    let playingUI client n = with div_ [class_ "flex flex-col"] do
            with div_ [class_ "mb-3"] do
                "Voting for: "
                b_ $ toHtml n
            div_ do
                renderPlayers \endpoint status -> case status of
                    Thinking -> " thinking about it..."
                    Voted vote
                        | client.endpoint == endpoint -> " your vote is: " <> toHtml vote
                        | otherwise -> " has voted!"

            let makeCard (value :: VoteValue) = do
                    withTrigger "click" ctx.wid "vote" ["card" .= value] button_ [class_ "mx-2 border w-16 py-1"] (toHtml value)
            with div_ [class_ "flex flex-wrap gap-2 mt-3"] do
                traverse_ makeCard $ (Vote <$> validVotes) <> [Unknown]

            with div_ [class_ "flex flex-row gap-2 text-center justify-center content-place-center pt-2"] do
                withTrigger_ "click" ctx.wid "reset" button_ [class_ btnRedClass] "Reset"
                withTrigger_ "click" ctx.wid "complete" button_ [class_ btnGreenClass] "Complete"

    let requestor :: Maybe AppID
        requestor = do
            argv <- ctx.argv
            argv ^? key "requestor" . _JSON

    let finishedUI txt = do
            with div_ [class_ "flex flex-col"] do
                with div_ [class_ "mb-3"] do
                    "Good game, here are the result for: "
                    b_ $ toHtml txt
                renderPlayers \_ status -> case status of
                    Thinking -> " has not voted!"
                    Voted v -> " -> " <> toHtml v
                finalResult <- voteSuggestion . getVotes <$> lift (readTVar state.players)
                with div_ [class_ "mt-3"] do
                    "PokerPlanner suggests: "
                    b_ $ toHtml finalResult
                with div_ [class_ "flex flex-row gap-2 text-center justify-center content-place-center pt-2"] do
                    withTrigger "click" ctx.wid "new-game" ["name" .= txt] button_ [class_ btnBlueClass] "Revote"
                    case requestor of
                        Just wid -> case finalResult of
                            Vote value -> withTrigger "click" wid "poker-result" ["value" .= value, "story" .= txt, "wid" .= ctx.wid] button_ [class_ btnGreenClass] "Submit"
                            _ -> pure ()
                        Nothing -> withTrigger_ "click" ctx.wid "reset" button_ [class_ btnGreenClass] "Next Game"

    let mountUI :: DisplayClient -> HtmlT STM ()
        mountUI client = with div_ [wid_ ctx.wid "w", style_ "z-index: 9001"] do
            status <- lift (readTVar state.status)
            case status of
                WaitingForGame -> waitingUI
                Playing txt -> playingUI client txt
                Finished txt -> finishedUI txt

    forever do
        atomically (readPipe ctx.pipe) >>= \case
            AppDisplay (UserJoined client) -> do
                atomically do
                    modifyTVar' state.players (Map.insert client.endpoint (Player client Thinking))
                clientsDrawT ctx.shared.clients mountUI
            AppDisplay (UserLeft client) -> do
                atomically $ modifyTVar' state.players (Map.delete client.endpoint)
            AppTrigger ev -> case ev.trigger of
                "new-game" -> case ev.body ^? key "name" . _JSON of
                    Just name -> do
                        atomically do
                            writeTVar state.status (Playing name)
                            modifyTVar' state.players (fmap $ \(Player client _) -> Player client Thinking)
                        clientsDrawT ctx.shared.clients mountUI
                    Nothing -> logError "Missing name" ["ev" .= ev]
                "vote" -> case ev.body ^? key "card" . _JSON of
                    Just vote -> do
                        atomically $ modifyTVar' state.players (Map.insert ev.client.endpoint (Player ev.client (Voted vote)))
                        clientsDrawT ctx.shared.clients mountUI
                    Nothing -> logError "Missing vote" ["ev" .= ev]
                "reset" -> do
                    atomically $ writeTVar state.status WaitingForGame
                    clientsDrawT ctx.shared.clients mountUI
                "complete" -> do
                    atomically $ modifyTVar state.status \case
                        Playing name -> Finished name
                        _ -> Finished "unknown"
                    clientsDrawT ctx.shared.clients mountUI
                _ -> logError "Unknown ev" ["ev" .= ev]
            _ -> pure ()
