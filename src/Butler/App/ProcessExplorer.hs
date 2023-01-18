module Butler.App.ProcessExplorer where

import Butler.Prelude

import Data.Aeson (Value (Number))

import Butler.Clock
import Butler.Desktop
import Butler.GUI
import Butler.Logger
import Butler.Processor
import Butler.Window

data PEState = PEAll | PEScopped Pid

renderPE :: TVar PEState -> WinID -> ProcessIO (HtmlT STM ())
renderPE stV wid = do
    os <- asks os
    st <- readTVarIO stV
    let startingPid = case st of
            PEAll -> Pid 1
            PEScopped p -> p
    let renderProcessM pid = do
            processM <- lift (getProcess os.processor pid)
            li_ do
                maybe "ghost" renderProcess processM
        renderProcess process = do
            status <- lift (readTVar process.status)
            with ul_ [class_ "pl-2 border-solid rounded border-l-2 border-slate-500"] do
                li_ do
                    trigger "ri-focus-3-fill text-red-500" "ps-kill" [("pid", Number (fromInteger $ toInteger $ process.pid))] "Kill the process"
                    toHtml (show process)
                    " | "
                    toHtml (show status)
                    childs <- lift (readTVar process.childs)
                    traverse_ renderProcess (reverse childs)
    pure $
        with div_ [id_ (withWID wid "w"), class_ "bg-slate-100 truncate h-full relative"] do
            topRightMenu
                [ with
                    div_
                    [class_ "cursor-pointer bg-grey-200 bg-opacity-90", wsSend, id_ (withWID wid "ps-toggle")]
                    case st of
                        PEAll -> "desktop scope"
                        PEScopped _ -> "show all"
                ]
            renderProcessM startingPid
  where
    trigger cls name vals title =
        with
            i_
            [ id_ (withWID wid name)
            , encodeVal vals
            , hxTrigger_ "click"
            , wsSend
            , title_ title
            , class_ (cls <> " pr-1 cursor-pointer")
            ]
            mempty

peApp :: Desktop -> WinID -> ProcessIO GuiApp
peApp desktop wid = do
    stV <- newTVarIO (PEScopped desktop.env.process.pid)
    newGuiApp "ps" Nothing (const $ renderPE stV wid) (scopeTriggers wid ["ps-kill", "ps-toggle"]) \app -> do
        spawnThread_ $ forever do
            ev <- atomically $ readPipe app.events
            case ev.body ^? key "pid" . _Integer of
                Just pid -> do
                    logInfo "Killing" ["pid" .= pid]
                    void $ killProcess (Pid (unsafeFrom pid))
                Nothing -> case withoutWID ev.trigger of
                    "ps-toggle" -> do
                        st <- readTVarIO stV
                        atomically $ writeTVar stV $ case st of
                            PEAll -> PEScopped desktop.env.process.pid
                            PEScopped _ -> PEAll
                        broadcastHtmlT desktop =<< renderPE stV wid
                    _ -> logError "unknown event" ["ev" .= ev]
        forever do
            os <- asks os
            sysEvent <- atomically =<< (waitLog os.logger 10_000 isProcess)
            case sysEvent of
                WaitCompleted{} -> do
                    broadcastHtmlT desktop =<< renderPE stV wid
                WaitTimeout{} -> pure ()
  where
    isProcess = \case
        ProcessCreated{} -> True
        ProcessStopped{} -> True
        _ -> False
