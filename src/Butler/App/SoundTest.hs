module Butler.App.SoundTest where

import Butler.Prelude
import Codec.EBML qualified as EBML
import Data.ByteString qualified as BS
import UnliftIO.Process qualified as Process

import Butler.Clock
import Butler.Desktop
import Butler.GUI
import Butler.Logger
import Butler.Session
import Butler.SoundBlaster
import Butler.Window

data TestState
    = Pending
    | DelayPlayback Process DisplayClient
    | Streaming Process

localStream :: SoundCard -> WinID -> ProcessIO Process
localStream sc wid = spawnProcess "recoder" do
    soundChannel <- atomically (startSoundChannel sc wid "test-streaming")
    logInfo "Running recorder" ["cmd" .= show cmd]
    Process.withCreateProcess cmd (processHandler soundChannel) `finally` do
        logInfo "Stopping recorder!" ["chan" .= soundChannel.id]
        atomically (stopSoundChannel sc soundChannel)
  where
    processHandler soundChannel _stdin (Just pStdout) (Just pStderr) _process = do
        spawnThread_ $ forever do
            buf <- liftIO (BS.hGetLine pStdout)
            logTrace "recorder" ["stdout" .= BSLog buf]

        let
            readStream sr = do
                buf <- liftIO (BS.hGet pStderr 512)
                case EBML.feedReader buf sr of
                    Left e
                        | buf == "" -> logInfo "stream stopped" ["chan" .= soundChannel.id]
                        | otherwise -> logError "stream decode failure" ["err" .= e]
                    Right (mFrame, newSR) -> do
                        atomically (feedChannel sc soundChannel buf mFrame)
                        readStream newSR

        readStream EBML.newStreamReader
    processHandler _ _ _ _ _ = error "Invalid process handler callback"
    cmd = cmdProc{Process.std_out = Process.CreatePipe, Process.std_err = Process.CreatePipe}
    cmdProc =
        Process.proc
            "gst-launch-1.0"
            ["pulsesrc", "!", "audioconvert", "!", "opusenc", "!", "webmmux", "!", "fdsink", "fd=2"]

delayPlayback :: SoundCard -> WinID -> DisplayClient -> ProcessIO Process
delayPlayback sc wid client = spawnProcess "delayer" do
    audioEventsChan <- atomically (newReaderChan sc.events)
    playbackChannel <- atomically (startSoundChannel sc wid "test-playback")
    let
        readStream = forever do
            ev <- atomically (readTChan audioEventsChan)
            case ev of
                SoundReceiveEvent recvClient buf mFrame | recvClient.process.pid == client.process.pid -> do
                    atomically $ feedChannel sc playbackChannel buf mFrame
                _ -> pure ()

    atomically (startClientRecorder wid client)

    readStream `finally` do
        logInfo "Stopping delayer!" []
        atomically do
            stopSoundChannel sc playbackChannel
            stopSoundReceiver wid client

soundTestHtml :: WinID -> SoundCard -> TVar TestState -> HtmlT STM ()
soundTestHtml wid sc vState = with div_ [id_ (withWID wid "sound-test")] do
    lift (readTVar vState) >>= \case
        Pending ->
            ul_ do
                with li_ [class_ "flex flex-row m-1"] do
                    with span_ [class_ "grow"] mempty
                    with button_ [id_ (withWID wid "playback-test"), wsSend, hxTrigger_ "click", class_ "bg-blue-500 hover:bg-blue-700 text-white font-bold p-1 rounded"] "record"
                with li_ [class_ "flex flex-row m-1"] do
                    with span_ [class_ "grow"] mempty
                    with button_ [id_ (withWID wid "stream-test"), wsSend, hxTrigger_ "click", class_ "bg-blue-500 hover:bg-blue-700 text-white font-bold p-1 rounded"] "streaming"
        DelayPlayback _process client -> do
            div_ do
                "Now playing back: "
                toHtml (client.session.username)
            with button_ [id_ (withWID wid "stop"), wsSend, hxTrigger_ "click", class_ "bg-red-500 hover:bg-red-700 text-white font-bold p-1 rounded"] "stop"
        Streaming _process -> do
            span_ "Streaming "
            with button_ [id_ (withWID wid "stop"), wsSend, hxTrigger_ "click", class_ "bg-red-500 hover:bg-red-700 text-white font-bold p-1 rounded"] "stop"
    soundCardInfoHtml sc

soundTestApp :: Desktop -> WinID -> ProcessIO GuiApp
soundTestApp desktop wid = do
    vState <- newTVarIO Pending
    let setStatus = atomically . writeTVar vState

    let render = soundTestHtml wid desktop.soundCard vState
        draw = const $ pure render
        triggers = ["stop", "playback-test", "stream-test"]
        refreshRate = 160

    size <- newTVarIO (200, 164)

    audioEventsChan <- atomically (newReaderChan desktop.soundCard.events)

    newGuiAppWin wid "sound-test" (Just size) draw triggers \app -> do
        spawnThread_ $ forever do
            _ev <- atomically (readTChan audioEventsChan)
            broadcastHtmlT desktop (soundCardInfoHtml desktop.soundCard)

        forever do
            state <- readTVarIO vState
            waitTransaction refreshRate (readPipe app.events) >>= atomically >>= \case
                WaitTimeout -> pure ()
                WaitCompleted ev -> case state of
                    Pending -> do
                        logInfo "got ev" ["ev" .= ev]
                        case withoutWID ev.trigger of
                            "playback-test" -> do
                                atomically (lookupDataClient desktop ev.client) >>= \case
                                    Nothing -> logError "Couldn't find data client" ["client" .= ev.client]
                                    Just client -> do
                                        process <- delayPlayback desktop.soundCard wid client
                                        setStatus (DelayPlayback process client)
                            "stream-test" -> do
                                process <- localStream desktop.soundCard wid
                                setStatus (Streaming process)
                            _ -> logError "unknown trigger" ["ev" .= ev]
                        clientsBroadcast desktop.hclients render
                    DelayPlayback process _client -> case withoutWID ev.trigger of
                        "stop" -> do
                            logInfo "Stopping record" ["ev" .= ev]
                            void $ killProcess process.pid
                            setStatus Pending
                            clientsBroadcast desktop.hclients render
                        _ -> logError "unknown rec trigger" ["ev" .= ev]
                    Streaming process -> case withoutWID ev.trigger of
                        "stop" -> do
                            logInfo "Stopping streamming" ["ev" .= ev]
                            void $ killProcess process.pid
                            setStatus Pending
                            clientsBroadcast desktop.hclients render
                        _ -> logError "unknown rec trigger" ["ev" .= ev]
