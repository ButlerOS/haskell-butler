-- | GUI toolkit
module Butler.Display.GUI (
    WinID (..),
    GuiEvent (..),
    TriggerName (..),
    HtmxEvent (..),
    decodeTriggerName,

    -- * helpers
    with',
    hyper_,
    wsSend,
    encodeVal,
    topRightMenu,
    hideScript,
    showScript,
    websocketHtml,
    splashHtml,
    loginForm,

    -- * Widget
    renderToggle,

    -- * Re-exports
    makeAttribute,
) where

import Butler.Prelude
import Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import Data.Aeson.Types (Pair)
import Data.Text qualified as Text
import Data.Text.Read qualified as Text

import Butler.Display.Client
import Butler.Display.Session

data HtmxEvent = HtmxEvent
    { trigger :: Text
    , body :: Value
    }

instance FromJSON HtmxEvent where
    parseJSON = withObject "HtmxEvent" $ \obj -> do
        headers <- obj .: "HEADERS"
        trigger <- headers .: "HX-Trigger"
        pure (HtmxEvent trigger (Object obj))

{- | Remove winID from trigger name

>>> decodeTriggerName "toggle"
Nothing
>>> decodeTriggerName "toggle-1"
Just (1,"toggle")
-}
decodeTriggerName :: Text -> Maybe (WinID, TriggerName)
decodeTriggerName txt = case Text.decimal txtSuffix of
    Right (wid, "") -> Just (WinID wid, TriggerName (Text.dropEnd 1 txtPrefix))
    _ -> Nothing
  where
    (txtPrefix, txtSuffix) = Text.breakOnEnd "-" txt

data GuiEvent = GuiEvent
    { client :: DisplayClient
    , trigger :: TriggerName
    , body :: Value
    }

instance ToJSON GuiEvent where
    toJSON (GuiEvent _ (TriggerName trigger) body) =
        object
            ["body" .= body, "trigger" .= trigger]

newtype TriggerName = TriggerName Text
    deriving newtype (Eq, Ord, Show, Semigroup, IsString)
    deriving (ToJSON) via Text

instance From Natural TriggerName where
    from = TriggerName . from . show

newtype WinID = WinID Int
    deriving newtype (Eq, Ord, Show, ToJSON, Serialise)

with' :: With a => a -> Text -> a
with' x n = with x [class_ n]

wsSend :: Attribute
wsSend = makeAttribute "ws-send" ""

hyper_ :: Text -> Attribute
hyper_ = makeAttribute "_"

encodeVal :: [Pair] -> Attribute
encodeVal kv = hxVals_ $ decodeUtf8 (from $ encodeJSON $ object kv)

hideScript :: Text -> Text
hideScript name = "htmx.addClass(htmx.find('#" <> name <> "'), 'invisible'); " <> "htmx.addClass(htmx.find('#" <> name <> "'), 'absolute')"

showScript :: Text -> Text
showScript name = "htmx.removeClass(htmx.find('#" <> name <> "'), 'invisible'); " <> "htmx.removeClass(htmx.find('#" <> name <> "'), 'absolute')"

topRightMenu :: Monad m => [HtmlT m ()] -> HtmlT m ()
topRightMenu items = do
    with div_ [class_ "absolute -top-3 -right-2 z-99 bg-stone-100 bg-opacity-80"] do
        with i_ [class_ "peer ri-settings-3-fill p-1 float-right"] mempty
        with div_ [class_ "pt-3 pr-2 hidden peer-hover:flex hover:flex flex-col drop-shadow-lg items-end"] do
            -- with div_ [class_ "flex flex-col drop-shadow-lg items-end"] do
            traverse_ div_ items

-- | Create the htmx websocket root element
websocketHtml :: Text -> SessionID -> Html ()
websocketHtml pathPrefix sessionID = do
    let wsUrl = pathPrefix <> "/ws/htmx" <> queryArgs
    with div_ [id_ "display-ws", class_ "h-full", makeAttribute "hx-ext" "ws", makeAttribute "ws-connect" wsUrl] do
        with div_ [id_ "display-root", class_ "h-full"] mempty
        -- script to get extra websocket url from javascript
        script_ $ "globalThis.wsUrl = n => 'wss://' + window.location.host + '" <> pathPrefix <> "/ws/' + n + '" <> queryArgs <> "';"
  where
    queryArgs = "?session=" <> from sessionID

-- | Display the content in a splash screen
splashHtml :: Monad m => HtmlT m () -> HtmlT m ()
splashHtml content = do
    with div_ [id_ "display-lock", class_ "h-screen w-screen absolute bg-gray-100 flex flex-col"] do
        with div_ [class_ "basis-1/6 flex bg-sky-600 border-sky-800 border-b-8"] mempty

        with div_ [class_ "grow flex bg-sky-200 flex-col justify-center"] do
            with div_ [class_ "flex flex-row justify-center"] do
                with div_ [class_ "p-3 rounded"] do
                    content

        with div_ [class_ "basis-1/6 flex bg-sky-600 border-sky-800 border-t-8"] mempty

loginForm :: Text -> Text -> [Pair] -> Html ()
loginForm pathPrefix title attr = do
    with form_ [id_ "login-form", hxPost_ (pathPrefix <> "/login"), encodeVal attr] do
        with div_ [class_ "font-semibold pb-2 flex flex-row justify-center"] do
            toHtml title
        with (input_ mempty) [name_ "username", type_ "text", placeholder_ "What is your name?"]

renderToggle :: Text -> [Attribute] -> Bool -> Text -> Text -> HtmlT STM ()
renderToggle icon attrs enabled onScript offScript = do
    let bg = if enabled then "bg-stone-400" else "bg-stone-800"
    with
        i_
        ( [ class_ $ icon <> " rounded-xl px-0.5 text-bold text-xl cursor-pointer " <> bg
          , wsSend
          , hxTrigger_ "click"
          , encodeVal ["running" .= enabled]
          ]
            <> attrs
        )
        do
            script_ $ if enabled then onScript else offScript
