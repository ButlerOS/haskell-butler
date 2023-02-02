-- | GUI toolkit
module Butler.GUI (
    GuiApp (..),
    emptyDraw,
    pureDraw,
    DrawHtml,
    GuiEvent (..),
    TriggerName (..),
    with',
    hyper_,
    wsSend,
    encodeVal,
    withWindow,
    button,
    topRightMenu,
    popup,
    hideScript,
    showScript,
    websocketHtml,
    splashHtml,
    loginForm,

    -- * Re-exports
    makeAttribute,
    module Butler.OS,
    module Butler.DisplayClient,
    module Butler.Pipe,
    module Lucid,
    module Lucid.Htmx,
) where

import Butler.Prelude
import Data.Aeson (Value (Number))
import Data.Text qualified as Text

import Lucid
import Lucid.Base (makeAttribute)
import Lucid.Htmx

import Butler.DisplayClient
import Butler.OS
import Butler.Pipe
import Butler.Session
import Data.Aeson.Types (Pair)

type DrawHtml = DisplayClient -> ProcessIO (HtmlT STM ())

pureDraw :: HtmlT STM () -> DrawHtml
pureDraw = const . pure

emptyDraw :: DrawHtml
emptyDraw = const $ pure mempty

data GuiApp = GuiApp
    { process :: Process
    , triggers :: Set TriggerName
    , draw :: DrawHtml
    , drawMenu :: DrawHtml
    , drawTray :: DrawHtml
    , events :: Pipe GuiEvent
    , size :: Maybe (TVar (Int, Int))
    }

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

with' :: With a => a -> Text -> a
with' x n = with x [class_ n]

wsSend :: Attribute
wsSend = makeAttribute "ws-send" ""

hyper_ :: Text -> Attribute
hyper_ = makeAttribute "_"

encodeVal :: [Pair] -> Attribute
encodeVal kv = hxVals_ $ decodeUtf8 (from $ encodeJSON $ object kv)

withWindow :: Monad m => Text -> Natural -> HtmlT m () -> HtmlT m ()
withWindow name wid body = do
    with div_ [class_ "border border-gray-500 overflow-auto m-2", id_ ("win-" <> from (show wid))] do
        with' h4_ "flex bg-gray-500 text-lg text-gray-100 font-medium p-1" do
            with' span_ "flex-grow" $ toHtml name
            winButton "min" "ri-subtract-line"
            winButton "max" "ri-fullscreen-line"
            winButton "close" "ri-close-line"
        body
  where
    winButton action cls =
        with
            i_
            [ id_ ("win-" <> from (show wid))
            , encodeVal [("wid", Number $ fromInteger $ toInteger wid), ("win-action", String action)]
            , hxTrigger_ "click"
            , wsSend
            , class_ $
                "mx-1 cursor-pointer " <> cls
            ]
            mempty

button :: Monad m => Text -> Text -> HtmlT m () -> HtmlT m ()
button bid cls =
    with
        i_
        [ id_ bid
        , wsSend
        , class_ $
            "mx-1 rounded cursor-pointer " <> cls
        ]

popup :: Text -> Text
popup t =
    "call Swal.fire({text: '"
        <> Text.replace "'" "\\'" t
        <> "', toast: true, position: 'bottom-end', timer: 5000, showConfirmButton: false, "
        <> "customClass: {popup: 'm-4'}})"

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
websocketHtml :: Text -> SessionID -> TabID -> Html ()
websocketHtml pathPrefix sessionID tabID = do
    let wsUrl = pathPrefix <> "/ws/htmx" <> queryArgs
    with div_ [id_ "display-ws", class_ "h-full", makeAttribute "hx-ext" "ws", makeAttribute "ws-connect" wsUrl] do
        with div_ [id_ "display-root", class_ "h-full"] mempty
        -- script to get extra websocket url from javascript
        script_ $ "globalThis.wsUrl = n => 'wss://' + window.location.host + '" <> pathPrefix <> "/ws/' + n + '" <> queryArgs <> "';"
  where
    queryArgs = "?session=" <> from sessionID <> "&tab=" <> from tabID

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
