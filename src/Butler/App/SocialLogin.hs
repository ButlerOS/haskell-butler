module Butler.App.SocialLogin (socialLoginApp) where

import Butler
import Butler.Display.Session

appHtml :: Monad m => AppID -> UserName -> HtmlT m ()
appHtml wid username =
    with div_ [wid_ wid "w", class_ "flex m-auto"] do
        with div_ [class_ "m-auto flex flex-col items-center justify-center gap-1"] do
            loginButton
            logoutButton
            div_ [] $ toHtml $ "Logged as " <> show username
  where
    loginButton  =
        div_
            [ wid_ wid "login-button"
            , wsSend
            , class_ "bg-slate-200 cursor-pointer pl-1"
            , hxTrigger_ "click"
            ]
            "Login with Google"
    logoutButton  =
        div_
            [ wid_ wid "logout-button"
            , wsSend
            , class_ "bg-slate-200 cursor-pointer pl-1"
            , hxTrigger_ "click"
            ]
            "Logout"

socialLoginApp :: App
socialLoginApp =
    (defaultApp "SocialLogin" startSocialLoginApp)
        { tags = fromList ["Utility"]
        , description = "A demo app to show a social login via OpenID Connect"
        , size = Just (164, 164)
        }

startSocialLoginApp :: AppContext -> ProcessIO ()
startSocialLoginApp ctx = do
    forever do
        res <- atomically $ readPipe ctx.pipe
        case res of
            AppDisplay (UserJoined client) -> do
                atomically $ do
                    username <- readTVar client.session.username
                    sendHtml client $ appHtml ctx.wid username
            AppTrigger ev -> case ev.trigger of
                TriggerName "login-button" -> do
                    sendsHtml ctx.shared.clients $
                        with div_ [wid_ ctx.wid "w"] do
                            script_ $ "window.location.href = \"/_login\""
                TriggerName "logout-button" -> do
                    sendsHtml ctx.shared.clients $
                        with div_ [wid_ ctx.wid "w"] do
                            script_ $ "window.location.href = \"/_logout\""
                _ -> logError "Unknown event" ["ev" .= ev]
            _ -> pure ()
