-- | GUI toolkit
module Butler.GUI (
    GuiApp (..),
    emptyDraw,
    newGuiApp,
    newGuiApp2,
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

    -- * Re-exports
    makeAttribute,
    module Butler.OS,
    module Butler.Display,
    module Butler.Pipe,
    module Lucid,
    module Lucid.Htmx,
) where

import Butler.Prelude
import Data.Aeson (Value (Number))
import Data.Set qualified as Set
import Data.Text qualified as Text

import Lucid
import Lucid.Base (makeAttribute)
import Lucid.Htmx

import Butler.Display
import Butler.OS
import Butler.Pipe
import Data.Aeson.Types (Pair)

type DrawHtml = DisplayClient -> ProcessIO (HtmlT STM ())

emptyDraw :: DrawHtml
emptyDraw = const $ pure mempty

newGuiApp :: ProgramName -> Maybe (TVar (Int, Int)) -> DrawHtml -> [TriggerName] -> (GuiApp -> ProcessIO ()) -> ProcessIO GuiApp
newGuiApp name size = newGuiApp2 name size emptyDraw emptyDraw

newGuiApp2 :: ProgramName -> Maybe (TVar (Int, Int)) -> DrawHtml -> DrawHtml -> DrawHtml -> [TriggerName] -> (GuiApp -> ProcessIO ()) -> ProcessIO GuiApp
newGuiApp2 name size drawTray drawMenu draw triggerNames cb = do
    events <- atomically newPipe
    let triggers = Set.fromList triggerNames
    let mkGuiApp process = GuiApp{..}
    mkGuiApp <$> spawnProcess ("app-" <> name) (cb . mkGuiApp =<< getSelfProcess)

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
encodeVal kv = hxVals_ $ unsafeFrom (encodeJSON $ object kv)

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
