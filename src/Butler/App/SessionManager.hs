module Butler.App.SessionManager where

import Data.Map.Strict qualified as Map
import Data.Text qualified as Text

import Butler
import Butler.App
import Butler.Display
import Butler.Display.Session

renderSM :: Display -> AppID -> DisplayClient -> HtmlT STM ()
renderSM display wid client = do
    isAdmin <- lift (readTVar client.session.admin)
    sessions <- Map.elems <$> lift (readTVar display.sessions.sessions)
    clients <- lift (readTVar display.clients)
    invites <- Map.toList <$> lift (readMemoryVar display.sessions.invitations)
    with div_ [id_ (withWID wid "w")] do
        with h2_ [class_ "font-semibold text-center"] "Connections"
        forM_ (Map.toList clients) \(session, displayClients) -> do
            with div_ [class_ "odd:bg-stone-100 even:bg-stone-200"] do
                with pre_ [class_ ""] (toHtml session)
                forM_ displayClients \dclient -> do
                    withTrigger
                        "click"
                        wid
                        "close-connection"
                        ["uuid" .= dclient.session.sessionID, "client" .= dclient.endpoint]
                        i_
                        [class_ "ri-focus-3-fill text-red-400 cursor-pointer top-0.5 relative", title_ "Close the connection"]
                        mempty
                    with span_ [class_ ""] do
                        toHtml (client.endpoint)
                    (send, recv) <- lift ((,) <$> readTVar client.send <*> readTVar client.recv)
                    with span_ [class_ "pl-1", title_ "upload/download in bytes"] do
                        toHtml (show send <> "/" <> show recv)

        with h2_ [class_ "font-semibold text-center pt-2 border-t-2 border-slate-700/50"] "Sessions"
        with' table_ "w-full" do
            thead_ $ tr_ do
                with th_ [class_ "pl-2"] "User"
                th_ "Admin"
                with th_ [class_ "text-center"] "Live"
                with th_ [class_ "text-right pr-5"] "ID"
            tbody_ $ forM_ sessions \session -> do
                with' tr_ "odd:bg-stone-100 even:bg-stone-200" do
                    td_ do
                        when (isAdmin || session.sessionID == client.session.sessionID) do
                            withTrigger
                                "click"
                                wid
                                "edit"
                                ["name" .= session.sessionID]
                                button_
                                [ class_ "relative rounded-xl text-sm text-slate-900 cursor-pointer"
                                ]
                                "📝"
                        toHtml =<< lift (readTVar session.username)
                    td_ do
                        isAdminSession <- lift (readTVar session.admin)
                        let confirm
                                | isAdminSession && client.session == session = Just "Are you sure?"
                                | otherwise = Nothing
                        input_ (butlerCheckbox wid "toggle-admin" ["uuid" .= session.sessionID] isAdminSession confirm)

                    with' td_ "text-center" (toHtml $ into @Text $ show $ length $ fromMaybe [] $ Map.lookup session.sessionID clients)
                    with' td_ "pl-2 text-right" do
                        toHtml (Text.takeWhile (/= '-') $ from session.sessionID)
                        when (isAdmin || session.sessionID == client.session.sessionID) do
                            with
                                i_
                                [ class_ "ri-delete-bin-2-fill text-red-500 px-2 cursor-pointer top-0.5 relative"
                                , onclick_ $ sendTriggerScriptConfirm wid "terminate-session" ["uuid" .= session.sessionID] (Just "Are you sure?")
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

renderEditForm :: Session -> AppID -> HtmlT STM ()
renderEditForm session wid = with div_ [id_ (withWID wid "w"), class_ "p-3"] do
    with form_ [wid_ wid "save", wsSend, hxTrigger_ "submit"] do
        with (input_ mempty) [name_ "name", value_ (into @Text session.sessionID), type_ "hidden"]
        div_ do
            with label_ [class_ "block mb-2 text-sm font-medium text-gray-900 dark:text-white"] "Username"
            username <- into @Text <$> lift (readTVar session.username)
            input_ [class_ inputClass, type_ "text", name_ "username", value_ username]

        with div_ [class_ "flex flex-row justify-center gap-3 items-center"] do
            button_ [class_ btnGreenClass, type_ "submit"] "Save"
            with button_ [class_ btnBlueClass, onclick_ (sendTriggerScript wid "listing" [])] "Cancel"

renderApp :: TVar AppState -> AppID -> DisplayClient -> HtmlT STM ()
renderApp state wid client =
    lift (readTVar state) >>= \case
        Listing display -> renderSM display wid client
        Editing session -> renderEditForm session wid

startSMApp :: AppContext -> ProcessIO ()
startSMApp ctx = do
    let display = ctx.shared.display
    state <- newTVarIO $ Listing display
    let refreshUI = clientsDrawT ctx.shared.clients (renderApp state ctx.wid)
    let handleGuiEvent ev = \case
            "listing" -> do
                atomically $ writeTVar state (Listing display)
                clientsDrawT ctx.shared.clients (renderSM display ctx.wid)
            "edit" -> case ev.body ^? key "name" . _JSON of
                Nothing -> logError "invalid edit action" ["ev" .= ev] >> pure mempty
                Just sessionID -> do
                    atomically (lookupSession display.sessions sessionID) >>= \case
                        Nothing -> logError "unknown session" ["id" .= sessionID] >> pure mempty
                        Just session -> do
                            atomically $ writeTVar state (Editing session)
                            refreshUI
            "save" -> do
                isAdmin <- readTVarIO ev.client.session.admin
                case (ev.body ^? key "name" . _JSON, ev.body ^? key "username" . _JSON) of
                    (Just sessionID, Just username)
                        | isAdmin || sessionID == ev.client.session.sessionID -> do
                            atomically (lookupSession display.sessions sessionID) >>= \case
                                Nothing -> logError "unknown session" ["id" .= sessionID] >> pure mempty
                                Just session -> do
                                    oldUsername <- readTVarIO session.username
                                    when (oldUsername /= username) do
                                        logInfo "Changing username" ["name" .= username, "session" .= session]
                                        void $ changeUsername display.sessions session username
                                    atomically $ writeTVar state (Listing display)
                                    refreshUI
                        | otherwise -> logError "Permission denied" ["ev" .= ev]
                    _ -> logError "invalid save action" ["ev" .= ev] >> pure mempty
            "new-invite" -> case ev.body ^? key "name" . _JSON of
                Nothing -> logError "invalid invite" [] >> pure mempty
                Just mInvite -> do
                    let invite = case mInvite of
                            "" -> "butler"
                            _ -> mInvite
                    newInvite display.sessions invite
                    refreshUI
            "delete-invite" -> case ev.body ^? key "uuid" . _JSON of
                Just invite -> do
                    atomically $ deleteInvite display.sessions invite
                    refreshUI
                Nothing -> pure mempty
            "terminate-session" -> do
                isAdmin <- readTVarIO ev.client.session.admin
                case ev.body ^? key "uuid" . _JSON of
                    Just session
                        | isAdmin || session == ev.client.session.sessionID -> do
                            deleteSession display.sessions session
                            pids <- atomically do
                                allClients <- fromMaybe [] . Map.lookup session <$> readTVar display.clients
                                pure ((.process.pid) <$> allClients)
                            logInfo "Terminating" ["pids" .= pids]
                            traverse_ killProcess pids
                            refreshUI
                        | otherwise -> logError "Permission denied" ["ev" .= ev]
                    Nothing -> pure ()
            "toggle-admin" -> withAdmin ev.client.session \adminSession -> do
                case ev.body ^? key "uuid" . _JSON of
                    Just sessionID -> do
                        atomically (lookupSession display.sessions sessionID) >>= \case
                            Just session -> do
                                newAdmin <- not <$> readTVarIO session.admin
                                logInfo "User admin changed" ["session" .= session, "admin" .= newAdmin]
                                setAdmin display.sessions adminSession session newAdmin
                                refreshUI
                            Nothing -> pure ()
                    Nothing -> pure ()
            "close-connection" -> do
                isAdmin <- readTVarIO ev.client.session.admin
                case (,) <$> ev.body ^? key "client" . _JSON <*> ev.body ^? key "uuid" . _JSON of
                    Just (endpoint, uuid)
                        | isAdmin || endpoint == ev.client.endpoint -> do
                            clientM <- atomically $ getClient display uuid endpoint
                            case clientM of
                                Just client -> void $ killProcess client.process.pid
                                Nothing -> logError "Unknown client" ["endpoint" .= endpoint]
                            refreshUI
                        | otherwise -> logError "Permission denied" ["ev" .= ev]
                    Nothing -> do
                        logError "close-connection missing client and/or uuid key" ["ev" .= ev]
            _ -> logError "unknown ev" ["ev" .= ev]
    forever do
        atomically (readPipe ctx.pipe) >>= \case
            AppDisplay (UserJoined client) -> atomically $ sendHtml client (renderApp state ctx.wid client)
            AppTrigger ge -> handleGuiEvent ge ge.trigger
            _ -> pure ()
