module Butler.Service.SshAgent (sshAgentService) where

import Butler
import Butler.Core.NatMap qualified as NM
import Butler.Frame

sshAgentService :: Service
sshAgentService =
    Service $
        (defaultApp "ssh-agent" startApp)
            { tags = fromList ["Utility"]
            , description = "Share ssh secrets"
            }

data Request = Request
    { socket :: Socket
    , provider :: TMVar DisplayClient
    }

startApp :: AppContext -> ProcessIO ()
startApp ctx = do
    providers <- atomically newDisplayClients
    requests <- atomically (NM.newNatMap @Request)

    let withRequest reqID context cb =
            atomically (NM.lookup requests reqID) >>= \case
                Nothing -> logError "Unknown conn" context
                Just req -> cb req

    let handler sock = void $ spawnThread do
            logInfo "new request" []
            -- Register the request
            request <- atomically (Request sock <$> newEmptyTMVar)
            reqID <- atomically (NM.add requests request)

            -- TODO: Ask provider to accept the request

            baton <- newEmptyTMVarIO
            void $ spawnThread do
                -- forward data to the provider
                provider <- atomically (readTMVar request.provider)
                fix \loop -> do
                    buf <- sktRecv sock 4096
                    if buf == mempty
                        then atomically (putTMVar baton ())
                        else do
                            atomically $ sendBinary provider (from buf)
                            loop

            -- wait for the completion
            res <- atomically =<< waitTransaction 30_000 (takeTMVar baton)
            case res of
                WaitTimeout -> logError "ssh-agent-request timeout" []
                _ -> pure ()

            -- TODO: forward close request to provider

            -- Deregister the request
            atomically (NM.delete requests reqID)

        mountUI = "TODO: <insert-agent-script>"

    spawnThread_ (unixService "/tmp/butler.sock" handler)
    forever do
        atomically (readPipe ctx.pipe) >>= \case
            ae@(AppDisplay (UserConnected{})) -> sendHtmlOnConnect mountUI ae
            AppDisplay (UserDisconnected _ client) -> atomically (delClient providers client)
            AppTrigger ev -> case ev.trigger of
                "new-provider" -> do
                    logInfo "new provider" ["client" .= ev.client]
                    atomically (addClient providers ev.client)
                "accept-request" -> case ev.body ^? key "req" . _JSON of
                    Nothing -> logError "Invalid accept" ["ev" .= ev]
                    Just sockID -> withRequest sockID ["trigger" .= ev] \request ->
                        unlessM (atomically $ tryPutTMVar request.provider ev.client) do
                            logError "Request already accepted" ["ev" .= ev]
                _ -> logError "Unknown trigger" ["ev" .= ev]
            AppData de -> case decodeMessage de.buffer of
                Just (into @Natural -> reqID, buf) ->
                    withRequest reqID ["data" .= de] \request -> do
                        atomically (tryReadTMVar request.provider) >>= \case
                            Just provider | provider.endpoint == de.client.endpoint -> do
                                sktSendAll request.socket buf
                            _ -> pure ()
                _ -> logError "Unknown data" ["ev" .= de]
