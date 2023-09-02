module Butler.App.SocialLogin (socialLoginApp) where

import Butler
import Butler.App (withEvent)
import Butler.Core.Dynamic (getSharedDynamic)
import Butler.Display.Session

newtype APPState = APPState Text deriving (Generic)

instance Serialise APPState

appHtml :: Monad m => AppID -> UserName -> Maybe SessionProvider -> Text -> HtmlT m ()
appHtml wid username mProvider secretText =
    div_ [wid_ wid "w", class_ "flex m-auto"] do
        div_ [class_ "m-auto flex flex-col items-center justify-center gap-1"] do
            case mProvider of
                Just provider -> do
                    logoutButton
                    div_ [] $ toHtml $ "Your are in a logged session " <> show provider <> " and your username is: " <> show username
                Nothing -> do
                    loginButton
                    div_ [] $ toHtml $ "Your are in a guest session and your username is: " <> show username
            deleteAccountButton
            div_ [] "Your secret text is stored in your session. You can update it and see that it is persited by session."
            div_ [class_ "flex flex-row gap-2"] do
                withEvent wid "setSecret" [] $ do
                    form_ $ do
                        label_ "Your secret text: "
                        input_ [type_ "text", name_ "secret-text", value_ secretText, size_ "30"]
                        saveButton
            div_ [id_ "saved-script"] ""
            div_ [id_ "saved"] ""
  where
    button :: Monad m => Text -> Text -> HtmlT m ()
    button bId label =
        div_
            [ wid_ wid bId
            , wsSend
            , class_ "bg-slate-200 cursor-pointer pl-1"
            , hxTrigger_ "click"
            ]
            $ toHtml label
    loginButton = button "login-button" "Login with Google"
    logoutButton = button "logout-button" "Logout"
    deleteAccountButton = button "delete-account-button" "Delete my session/account"
    saveButton =
        button_
            [ wid_ wid "save-button"
            , wsSend
            , type_ "submit"
            , class_ "cursor-pointer pl-1 border-2"
            , hxTrigger_ "click"
            ]
            "Save"

hideAfter :: Text
hideAfter =
    [raw|
setTimeout(() => {
  const elm = document.getElementById('saved');
  elm.style.display = 'none';
}, 1500);
|]

socialLoginApp :: App
socialLoginApp =
    (defaultApp "SocialLogin" startSocialLoginApp)
        { tags = fromList ["Utility"]
        , description = "A demo app to show a social login via OpenID Connect"
        , size = Just (164, 164)
        }

startSocialLoginApp :: AppContext -> ProcessIO ()
startSocialLoginApp ctx = do
    let appState = APPState "My default secret text as guest"
        memAddr = "app-state-" <> showT ctx.wid <> ".bin"
    -- Use getSharedDynamic to use a single list of secret for the workspace
    (_, state) <- getSharedDynamic ctx.shared.dynamics "social-secret" do
        newProcessMemory (from memAddr) (pure appState)
    forever do
        res <- atomically $ readPipe ctx.pipe
        case res of
            AppDisplay (UserJoined client) -> do
                atomically $ do
                    username <- readTVar client.session.username
                    mProvider <- readTVar client.session.provider
                    APPState secretText <- readMemoryVar state
                    sendHtml client $ appHtml ctx.wid username mProvider secretText
            AppTrigger ev -> case ev.trigger of
                TriggerName "login-button" -> redirectTo "/_login"
                TriggerName "logout-button" -> redirectTo "/_logout"
                TriggerName "delete-account-button" -> redirectTo "/_delete"
                TriggerName "save-button" -> do
                    case ev.body ^? key "secret-text" . _String of
                        Just secretText -> do
                            atomically $ modifyMemoryVar state $ const $ APPState secretText
                            sendsHtml ctx.shared.clients $ do
                                div_ [id_ "saved", class_ "bg-green-200"] "saved !"
                                div_ [id_ "saved-script"] $ script_ hideAfter
                        Nothing -> pure ()
                _ -> logError "Unknown event" ["ev" .= ev]
            _ -> pure ()
  where
    redirectTo :: Text -> ProcessIO ()
    redirectTo dest =
        sendsHtml ctx.shared.clients $
            with div_ [wid_ ctx.wid "w"] do
                script_ $ "window.location.href = \"" <> dest <> "\""
