module Butler.App.SoundTest (soundTestApp) where

import Codec.EBML qualified as EBML
import Data.ByteString qualified as BS
import UnliftIO.Process qualified as Process

import Butler
import Butler.Logger
import Butler.Session
import Butler.SoundBlaster

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
            logDebug "recorder" ["stdout" .= BSLog buf]

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

    atomically (startClientRecorder sc wid client)

    readStream `finally` do
        logInfo "Stopping delayer!" []
        atomically do
            stopSoundChannel sc playbackChannel
            stopSoundReceiver sc wid client

soundTestHtml :: WinID -> SoundCard -> TVar TestState -> HtmlT STM ()
soundTestHtml wid sc vState = with div_ [id_ (withWID wid "w")] do
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

soundTestApp :: SoundCard -> App
soundTestApp sc =
    App
        { name = "sound-test"
        , tags = fromList ["Utility", "Sound"]
        , description = "Test audio stream"
        , size = Just (200, 164)
        , start = startSoundTest sc
        }

startSoundTest :: SoundCard -> AppStart
startSoundTest soundCard clients wid pipeAE = do
    vState <- newTVarIO Pending
    let setStatus = atomically . writeTVar vState

    let render = soundTestHtml wid soundCard vState

    audioEventsChan <- atomically (newReaderChan soundCard.events)

    spawnThread_ $ forever do
        _ev <- atomically (readTChan audioEventsChan)
        sendsHtml clients (soundCardInfoHtml soundCard)

    forever do
        ae <- atomically (readPipe pipeAE)
        state <- readTVarIO vState
        case ae of
            AppDisplay _ -> sendHtmlOnConnect render ae
            AppTrigger ev -> case state of
                Pending -> do
                    logInfo "got ev" ["ev" .= ev]
                    case withoutWID ev.trigger of
                        "playback-test" -> do
                            process <- delayPlayback soundCard wid ev.client
                            setStatus (DelayPlayback process ev.client)
                        "stream-test" -> do
                            process <- localStream soundCard wid
                            setStatus (Streaming process)
                        _ -> logError "unknown trigger" ["ev" .= ev]
                    sendsHtml clients render
                DelayPlayback process _client -> case withoutWID ev.trigger of
                    "stop" -> do
                        logInfo "Stopping record" ["ev" .= ev]
                        void $ killProcess process.pid
                        setStatus Pending
                        sendsHtml clients render
                    _ -> logError "unknown rec trigger" ["ev" .= ev]
                Streaming process -> case withoutWID ev.trigger of
                    "stop" -> do
                        logInfo "Stopping streamming" ["ev" .= ev]
                        void $ killProcess process.pid
                        setStatus Pending
                        sendsHtml clients render
                    _ -> logError "unknown rec trigger" ["ev" .= ev]
            _ -> pure ()
