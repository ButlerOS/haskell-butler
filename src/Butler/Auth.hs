-- | This module contains the logic to serve App with Display.
module Butler.Auth where

import Butler.Auth.Guest
import Butler.Display
import Butler.Prelude
import Lucid.XStatic

publicDisplayApp :: DisplayApplication
publicDisplayApp = DisplayApplication auth
  where
    auth xfiles = const . pure . guestAuthApp $ htmlMain xfiles "Standalone GUI"

htmlMain :: [XStaticFile] -> Text -> Html ()
htmlMain xfiles title = do
    doctypehtml_ do
        head_ do
            title_ (toHtml title)
            meta_ [charset_ "utf-8"]
            meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
            xstaticScripts xfiles

        with body_ [class_ "font-mono cursor-default bg-stone-100 h-screen"] do
            with div_ [id_ "display-ws", class_ "h-full", makeAttribute "hx-ext" "ws", makeAttribute "ws-connect" "/ws/htmx"] do
                with div_ [id_ "display-wins"] do
                    mempty
