-- | This module contains the logic to serve App with Display.
module Butler.Auth where

import Butler.Auth.Guest
import Butler.Auth.OIDC
import Butler.Display
import Butler.Frame
import Butler.Prelude
import Lucid.XStatic

newtype PageTitle = PageTitle Text
    deriving newtype (IsString, ToHtml)

newtype PageDesc = PageDesc Text
    deriving newtype (IsString)

publicDisplayApp :: PageTitle -> Maybe PageDesc -> DisplayApplication
publicDisplayApp appTitle appDescM = DisplayApplication auth
  where
    auth xfiles sessions = guestAuthApp sessions $ htmlMain xfiles appTitle appDescM

publicOIDCDisplayApp :: OIDCClientID -> OIDCClientSecret -> PageTitle -> Maybe PageDesc -> DisplayApplication
publicOIDCDisplayApp client_id client_password appTitle appDescM = DisplayApplication auth
  where
    auth xfiles sessions =
        oIDCAuthApp sessions public_url client_id client_password $
            htmlMain xfiles appTitle appDescM
    public_url = "https://localhost:8080/"

htmlMain :: [XStaticFile] -> PageTitle -> Maybe PageDesc -> Html () -> Html ()
htmlMain xfiles title descM body = do
    doctypehtml_ do
        head_ do
            title_ (toHtml title)
            forM_ descM (\(PageDesc desc) -> meta_ [name_ "description", content_ desc])
            meta_ [charset_ "utf-8"]
            meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
            xstaticScripts xfiles
            script_ butlerHelpersScript
            body
