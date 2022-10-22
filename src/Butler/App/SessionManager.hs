module Butler.App.SessionManager where

import Butler.Prelude

import Data.Text qualified as Text
import Data.UUID qualified as UUID

import Data.Map.Strict qualified as Map

import Butler.Desktop
import Butler.GUI
import Butler.Memory
import Butler.Session

newtype SessionManager = SessionManager
    { visible :: TVar Bool
    }

hideSM :: Monad m => HtmlT m ()
hideSM = bodyHtml "hidden" mempty

bodyHtml :: Monad m => Text -> HtmlT m () -> HtmlT m ()
bodyHtml cls = with div_ [id_ "sm-list", class_ cls]

renderSM :: SessionManager -> Display -> HtmlT STM ()
renderSM sm display = do
    visible <- lift (readTVar sm.visible)
    if visible
        then doRender
        else hideSM
  where
    doRender = do
        sessions <- Map.elems <$> lift (readMemoryVar display.sessions.sessions)
        clients <- lift (readTVar display.clients)
        invites <- Map.toList <$> lift (readMemoryVar display.sessions.invitations)
        bodyHtml "absolute bottom-8 pl-1 shadow-inner pt-1 right-0 bg-slate-700 bg-opacity-50 z-50" do
            with div_ [class_ "bg-stone-200"] do
                with h2_ [class_ "font-semibold text-center"] "Connections"
                forM_ (Map.toList clients) \(session, displayClients) -> do
                    with div_ [class_ "odd:bg-stone-100 even:bg-stone-200"] do
                        with pre_ [class_ ""] (toHtml session)
                        forM_ displayClients \client -> do
                            with
                                i_
                                [ id_ "close-connection"
                                , encodeVal ["uuid" .= into @Text client.session.sessionID, "client" .= client.endpoint]
                                , class_ "ri-focus-3-fill text-red-400 cursor-pointer top-0.5 relative"
                                , wsSend
                                , title_ "Close the connection"
                                ]
                                mempty
                            with span_ [class_ ""] do
                                toHtml (client.endpoint)
                            (send, recv) <- lift ((,) <$> readTVar client.send <*> readTVar client.recv)
                            with span_ [class_ "pl-1", title_ "upload/download in bytes"] do
                                toHtml (show send <> "/" <> show recv)

                with h2_ [class_ "font-semibold text-center pt-2 border-t-2 border-slate-700/50"] "Sessions"
                with' table_ "w-full" do
                    thead_ $ tr_ $ traverse_ th_ ["User", "Live", "ID"]
                    tbody_ $ forM_ sessions \session -> do
                        with' tr_ "odd:bg-stone-100 even:bg-stone-200" do
                            td_ (toHtml session.username)
                            with' td_ "text-right" (toHtml $ into @Text $ show $ length $ fromMaybe [] $ Map.lookup session.sessionID clients)
                            with' td_ "pl-2 text-right" do
                                toHtml (Text.takeWhile (/= '-') $ from session.sessionID)
                                with
                                    i_
                                    [ id_ "terminate-session"
                                    , encodeVal ["uuid" .= into @Text session.sessionID]
                                    , class_ "ri-delete-bin-2-fill text-red-500 px-2 cursor-pointer top-0.5 relative"
                                    , wsSend
                                    , hxTrigger_ "confirmed"
                                    , title_ "Delete the sessions"
                                    , hyper_ $
                                        Text.unlines
                                            [ "on click"
                                            , "call Swal.fire({title: 'Confirm', text:'Do you want to continue?'})"
                                            , "if result.isConfirmed trigger confirmed"
                                            ]
                                    ]
                                    mempty
                with h2_ [class_ "font-semibold text-center pt-2 border-t-2 border-slate-700/50"] "Invites"
                with' table_ "w-full" do
                    unless (null invites) do
                        thead_ $ tr_ $ traverse_ th_ ["Accepted", "ID"]
                        tbody_ $ forM_ invites \(invite, xs) -> do
                            with' tr_ "odd:bg-stone-100 even:bg-stone-200" do
                                td_ do
                                    toHtml $ into @Text $ show $ length xs
                                    with
                                        i_
                                        [ class_ "ri-clipboard-line top-0.5 relative pl-2"
                                        , hyper_ $
                                            "on click writeText(window.location.origin + '?invite="
                                                <> UUID.toText (coerce invite)
                                                <> "') into the navigator's clipboard "
                                                <> "then call Swal.fire({text: 'Invite URL copied to the clipboard', toast: true, position: 'bottom-end', timer: 5000, showConfirmButton: false, width: '430px'})"
                                        ]
                                        mempty
                                with' td_ "text-right" do
                                    toHtml invite
                                    with
                                        i_
                                        [ id_ "delete-invite"
                                        , encodeVal ["uuid" .= invite]
                                        , class_ "ri-delete-bin-2-fill text-red-500 px-2 cursor-pointer top-0.5 relative"
                                        , wsSend
                                        , title_ "Delete the invite"
                                        ]
                                        mempty
                with' div_ "w-full text-center" do
                    with
                        i_
                        [ class_ "relative rounded-xl text-2xl text-slate-900 ri-add-circle-fill cursor-pointer"
                        , hxTrigger_ "click"
                        , wsSend
                        , id_ "new-invite"
                        ]
                        mempty

smApp :: Desktop -> ProcessIO GuiApp
smApp desktop = do
    let display = desktop.display
    sm <- SessionManager <$> newTVarIO False
    let triggers = ["new-invite", "delete-invite", "terminate-session", "display-pulse", "close-connection"]
    newGuiApp "session-manager" Nothing (const $ pure $ renderSM sm display) triggers \app -> forever do
        ev <- atomically $ readPipe app.events
        resp <- case ev.trigger of
            "new-invite" -> do
                newInvite display.sessions
                pure $ renderSM sm display
            "delete-invite" -> case ev.body ^? key "uuid" . _JSON of
                Just invite -> atomically do
                    deleteInvite display.sessions invite
                    pure $ renderSM sm display
                Nothing -> pure mempty
            "terminate-session" -> case ev.body ^? key "uuid" . _JSON of
                Just session -> do
                    pids <- atomically do
                        deleteSession display.sessions session
                        clients <- fromMaybe [] . Map.lookup session <$> readTVar display.clients
                        pure ((.process.pid) <$> clients)
                    logInfo "Terminating" ["pids" .= pids]
                    traverse_ killProcess pids
                    pure $ renderSM sm display
                Nothing -> pure mempty
            "display-pulse" -> atomically do
                modifyTVar sm.visible not
                visible <- readTVar sm.visible
                pure $
                    if visible
                        then renderSM sm display
                        else hideSM
            "close-connection" -> case (,) <$> ev.body ^? key "client" . _JSON <*> ev.body ^? key "uuid" . _JSON of
                Just (endpoint, uuid) -> do
                    clientM <- atomically $ getClient display uuid endpoint
                    case clientM of
                        Just client -> void $ killProcess client.process.pid
                        Nothing -> logError "Unknown client" ["endpoint" .= endpoint]
                    pure $ renderSM sm display
                Nothing -> do
                    logError "close-connection missing client and/or uuid key" ["ev" .= ev]
                    pure mempty
            _ -> do
                logError "unknown ev" ["ev" .= ev]
                pure mempty
        sendHtml ev.client resp
