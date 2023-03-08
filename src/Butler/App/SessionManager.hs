module Butler.App.SessionManager where

import Data.Map.Strict qualified as Map
import Data.Text qualified as Text

import Butler
import Butler.Display
import Butler.Display.Session

renderSM :: Display -> WinID -> HtmlT STM ()
renderSM display wid = do
    sessions <- Map.elems <$> lift (readTVar display.sessions.sessions)
    clients <- lift (readTVar display.clients)
    invites <- Map.toList <$> lift (readMemoryVar display.sessions.invitations)
    with div_ [id_ (withWID wid "w")] do
        with h2_ [class_ "font-semibold text-center"] "Connections"
        forM_ (Map.toList clients) \(session, displayClients) -> do
            with div_ [class_ "odd:bg-stone-100 even:bg-stone-200"] do
                with pre_ [class_ ""] (toHtml session)
                forM_ displayClients \client -> do
                    with
                        i_
                        [ id_ (withWID wid "close-connection")
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
                    td_ do
                        with form_ [wid_ wid "edit", wsSend, hxTrigger_ "submit"] do
                            with
                                button_
                                [ class_ "relative rounded-xl text-sm text-slate-900 cursor-pointer"
                                , type_ "submit"
                                ]
                                "📝"

                            with (input_ mempty) [name_ "name", value_ (into @Text session.sessionID), type_ "hidden"]
                            toHtml =<< lift (readTVar session.username)
                    with' td_ "text-center" (toHtml $ into @Text $ show $ length $ fromMaybe [] $ Map.lookup session.sessionID clients)
                    with' td_ "pl-2 text-right" do
                        toHtml (Text.takeWhile (/= '-') $ from session.sessionID)
                        with
                            i_
                            [ wid_ wid "terminate-session"
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
                                        <> coerce @InviteID @Text invite
                                        <> "') into the navigator's clipboard "
                                        <> "then call Swal.fire({text: 'Invite URL copied to the clipboard', toast: true, position: 'bottom-end', timer: 5000, showConfirmButton: false, width: '430px'})"
                                ]
                                mempty
                        with' td_ "text-right" do
                            toHtml invite
                            with
                                i_
                                [ wid_ wid "delete-invite"
                                , encodeVal ["uuid" .= invite]
                                , class_ "ri-delete-bin-2-fill text-red-500 px-2 cursor-pointer top-0.5 relative"
                                , wsSend
                                , title_ "Delete the invite"
                                ]
                                mempty
        with' div_ "w-full text-center" do
            with form_ [wid_ wid "new-invite", wsSend, hxTrigger_ "submit"] do
                with div_ [class_ "flex flex-row mx-2 my-1"] do
                    with (input_ mempty) [id_ "invite-input", class_ inputClass, name_ "name", placeholder_ "invite-name", type_ "text"]
                    with
                        button_
                        [ class_ "relative rounded-xl text-2xl text-slate-900 ri-add-circle-fill cursor-pointer"
                        , type_ "submit"
                        ]
                        mempty

smApp :: App
smApp =
    (defaultApp "user-manager" startSMApp)
        { tags = fromList ["System"]
        , description = "Manage sessions"
        }

data AppState = Listing Display | Editing Session

renderEditForm :: Session -> WinID -> HtmlT STM ()
renderEditForm session wid = with div_ [id_ (withWID wid "w")] do
    with form_ [wid_ wid "save", wsSend, hxTrigger_ "submit"] do
        div_ do
            with label_ [class_ "block mb-2 text-sm font-medium text-gray-900 dark:text-white"] "Username"
            username <- into @Text <$> lift (readTVar session.username)
            with (input_ mempty) [name_ "name", value_ (into @Text session.sessionID), type_ "hidden"]
            with (input_ mempty) [class_ inputClass, name_ "username", type_ "text", value_ username]
        with div_ [class_ "flex flex-row items-center"] do
            button_
                [ class_ btnBlueClass
                , wsSend
                , wid_ wid "listing"
                ]
                "Cancel"
            button_
                [ class_ btnGreenClass
                , type_ "submit"
                ]
                "Save"

renderApp :: TVar AppState -> WinID -> HtmlT STM ()
renderApp state wid =
    lift (readTVar state) >>= \case
        Listing display -> renderSM display wid
        Editing session -> renderEditForm session wid

startSMApp :: AppContext -> ProcessIO ()
startSMApp ctx = do
    let clients = ctx.clients
        wid = ctx.wid
        display = ctx.shared.display
    state <- newTVarIO $ Listing display
    let handleGuiEvent ev = do
            resp <- case ev.trigger of
                "listing" -> do
                    atomically $ writeTVar state (Listing display)
                    pure $ renderSM display wid
                "edit" -> case ev.body ^? key "name" . _JSON of
                    Nothing -> logError "invalid edit action" ["ev" .= ev] >> pure mempty
                    Just sessionID -> do
                        atomically (lookupSession display.sessions sessionID) >>= \case
                            Nothing -> logError "unknown session" ["id" .= sessionID] >> pure mempty
                            Just session -> do
                                atomically $ writeTVar state (Editing session)
                                pure $ renderApp state wid
                "save" -> case (ev.body ^? key "name" . _JSON, ev.body ^? key "username" . _JSON) of
                    (Just sessionID, Just username) -> do
                        atomically (lookupSession display.sessions sessionID) >>= \case
                            Nothing -> logError "unknown session" ["id" .= sessionID] >> pure mempty
                            Just session -> do
                                _ <- changeUsername display.sessions session username
                                atomically $ writeTVar state (Listing display)
                                pure $ renderApp state wid
                    _ -> logError "invalid save action" ["ev" .= ev] >> pure mempty
                "new-invite" -> case ev.body ^? key "name" . _JSON of
                    Nothing -> logError "invalid invite" [] >> pure mempty
                    Just mInvite -> do
                        let invite = case mInvite of
                                "" -> "butler"
                                _ -> mInvite
                        newInvite display.sessions invite
                        pure $ renderSM display wid
                "delete-invite" -> case ev.body ^? key "uuid" . _JSON of
                    Just invite -> atomically do
                        deleteInvite display.sessions invite
                        pure $ renderSM display wid
                    Nothing -> pure mempty
                "terminate-session" -> case ev.body ^? key "uuid" . _JSON of
                    Just session -> do
                        deleteSession display.sessions session
                        pids <- atomically do
                            allClients <- fromMaybe [] . Map.lookup session <$> readTVar display.clients
                            pure ((.process.pid) <$> allClients)
                        logInfo "Terminating" ["pids" .= pids]
                        traverse_ killProcess pids
                        pure $ renderSM display wid
                    Nothing -> pure mempty
                "close-connection" -> case (,) <$> ev.body ^? key "client" . _JSON <*> ev.body ^? key "uuid" . _JSON of
                    Just (endpoint, uuid) -> do
                        clientM <- atomically $ getClient display uuid endpoint
                        case clientM of
                            Just client -> void $ killProcess client.process.pid
                            Nothing -> logError "Unknown client" ["endpoint" .= endpoint]
                        pure $ renderSM display wid
                    Nothing -> do
                        logError "close-connection missing client and/or uuid key" ["ev" .= ev]
                        pure mempty
                _ -> do
                    logError "unknown ev" ["ev" .= ev]
                    pure mempty
            sendsHtml clients resp
    forever do
        atomically (readPipe ctx.pipe) >>= \case
            ae@(AppDisplay _) -> sendHtmlOnConnect (renderApp state wid) ae
            AppTrigger ge -> handleGuiEvent ge
            _ -> pure ()
