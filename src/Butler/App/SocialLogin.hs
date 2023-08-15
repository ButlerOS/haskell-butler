module Butler.App.SocialLogin (socialLoginApp) where

import Butler
import Butler.Display.Session

appHtml :: Monad m => AppID -> UserName -> HtmlT m ()
appHtml wid username =
    with div_ [id_ (withWID wid "w"), class_ "flex m-auto"] do
        with div_ [class_ "m-auto flex flex-col items-center justify-center"] do
            loginButton "Click to social login"
            div_ [] $ toHtml $ "Logged as " <> show username
  where
    loginButton tz =
        div_
            [ id_ (withWID wid "social-login-button")
            , wsSend
            , class_ "bg-slate-200 cursor-pointer pl-1"
            , hxTrigger_ "click"
            , hxVals_ ("{\"v\": \"" <> tz <> "\"}")
            ]
            (toHtml tz)

socialLoginApp :: App
socialLoginApp =
    (defaultApp "SocialLogin" startSocialLoginApp)
        { tags = fromList ["Utility"]
        , description = "Display time"
        , size = Just (164, 164)
        }

startSocialLoginApp :: AppContext -> ProcessIO ()
startSocialLoginApp ctx = do
    forever do
        res <- atomically =<< waitTransaction 60_000 (readPipe ctx.pipe)
        case res of
            WaitTimeout{} -> pure ()
            WaitCompleted (AppDisplay (UserJoined client)) -> do
                atomically $ do
                    username <- readTVar client.session.username
                    sendHtml client $ appHtml ctx.wid username
            WaitCompleted (AppTrigger ev) -> case ev.body ^? key "v" . _String of
                Just "Click to social login" -> do
                    sendsHtml ctx.shared.clients $
                        with div_ [id_ "w-1"] do
                            script_ $ "window.location.href = " <> "\"https://localhost:8080/_login\""
                _ -> logError "Unknown event" ["ev" .= ev]
            WaitCompleted _ -> pure ()
