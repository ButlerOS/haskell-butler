module Butler.Service.SshAgent (sshAgentService) where

import Butler
import Butler.Core.NatMap qualified as NM
import Butler.Frame
import Butler.UnixShell

sshAgentService :: Isolation -> Service
sshAgentService isolation =
    Service $
        (defaultApp "ssh-agent" (startApp isolation))
            { tags = fromList ["Utility"]
            , description = "Share ssh secrets"
            }

data Request = Request
    { socket :: Socket
    , provider :: TMVar DisplayClient
    }

startApp :: Isolation -> AppContext -> ProcessIO ()
startApp isolation ctx = do
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

            let sendMessage = encodeMessage (from ctx.wid) . encodeMessage reqID
            sendsBinary ctx.shared.clients $ sendMessage ""

            baton <- newEmptyTMVarIO
            void $ spawnThread do
                -- forward data to the provider
                provider <- atomically (readTMVar request.provider)
                fix \loop -> do
                    buf <- sktRecv sock 4096
                    if buf == mempty
                        then atomically (putTMVar baton ())
                        else do
                            atomically $ sendBinary provider (sendMessage $ from buf)
                            loop

            -- wait for the completion
            res <- atomically =<< waitTransaction 30_000 (takeTMVar baton)
            case res of
                WaitTimeout -> logError "ssh-agent-request timeout" []
                _ -> pure ()

            -- TODO: forward close request to provider

            -- Deregister the request
            atomically (NM.delete requests reqID)

        mountUI =
            with div_ [wid_ ctx.wid "tray"] do
                script_ (sshAgentProvider ctx.wid)

    baseDir <- from . decodeUtf8 <$> getPath "rootfs"
    sktPath <- case isolation.runtime of
        None -> pure "/tmp/butler-agent.sock" -- TODO: make the path unique per desktop
        _ -> do
            let sktDir = baseDir </> "skt"
                sktPath = sktDir </> "agent.sock"
            liftIO $ createDirectoryIfMissing True sktDir
            pure sktPath

    spawnThread_ (unixService sktPath handler)
    forever do
        atomically (readPipe ctx.pipe) >>= \case
            AppDisplay (UserJoined client) -> atomically (sendHtml client mountUI)
            AppDisplay (UserLeft client) -> atomically (delClient providers client)
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
                Just (reqID, buf) ->
                    withRequest reqID ["data" .= de] \request -> do
                        atomically (tryReadTMVar request.provider) >>= \case
                            Just provider | provider.endpoint == de.client.endpoint -> do
                                sktSendAll request.socket buf
                            _ -> pure ()
                _ -> logError "Unknown data" ["ev" .= de]
            ev -> logError "Unexpected event" ["ev" .= ev]

sshAgentProvider :: AppID -> Text
sshAgentProvider wid =
    [raw|
async function setupSshAgentProvider(wid) {
  if (typeof butlerElectron !== "undefined" && await butlerElectron.hasSshAgent()) {
    sendTrigger(wid, "new-provider", {})

    butlerDataHandlers[wid] = buf => decodeDataMessage(buf, (chan, data) => {
      if (data.length == 0) {
        console.log("New ssh-agent request", chan)
        butlerElectron.connectSshAgent(chan, data => {
          sendBinaryMessage2(wid, chan, data)
        })
        sendTrigger(wid, "accept-request", {req: chan})
      } else {
        butlerElectron.sendSshAgent(chan, data)
      }
    })
  }
}
|]
        <> "\nsetupSshAgentProvider("
        <> showT wid
        <> ");"
