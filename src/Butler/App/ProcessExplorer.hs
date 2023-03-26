module Butler.App.ProcessExplorer where

import Butler
import Butler.Core
import Butler.Core.Logger

import Butler.Core.Processor
import Data.Aeson (Value (Number))

data PEState = PEAll | PEScopped Pid

renderPE :: OS -> TVar PEState -> AppID -> HtmlT STM ()
renderPE os stV wid = do
    st <- lift (readTVar stV)
    let startingPid = case st of
            PEAll -> Pid 1
            PEScopped p -> p
    let renderProcessM pid = do
            mProcess <- lift (lookupProcess os.processor pid)
            li_ do
                maybe "ghost" renderProcess mProcess
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

peApp :: App
peApp =
    (defaultApp "ps" startPE)
        { name = "ps"
        , tags = fromList ["System"]
        , description = "Process Explorer"
        }
  where
    startPE ctx = do
        let clients = ctx.shared.clients
            wid = ctx.wid
        stV <- newTVarIO (PEScopped ctx.shared.processEnv.process.pid)
        os <- asks os
        let doRender = renderPE os stV wid
            handleEvent ev =
                case ev.body ^? key "pid" . _Integer of
                    Just pid -> do
                        logInfo "Killing" ["pid" .= pid]
                        void $ killProcess (Pid (unsafeFrom pid))
                    Nothing -> case withoutWID ev.trigger of
                        "ps-toggle" -> do
                            st <- readTVarIO stV
                            atomically $ writeTVar stV $ case st of
                                PEAll -> PEScopped ctx.shared.processEnv.process.pid
                                PEScopped _ -> PEAll
                            sendsHtml clients doRender
                        _ -> logError "unknown event" ["ev" .= ev]
        spawnThread_ $ forever do
            atomically (readPipe ctx.pipe) >>= \case
                de@(AppDisplay _) -> sendHtmlOnConnect (renderPE os stV wid) de
                AppTrigger ev -> handleEvent ev
                _ -> pure ()

        forever do
            sysEvent <- atomically =<< waitLog os.logger 10_000 isProcess
            case sysEvent of
                WaitCompleted{} -> sendsHtml clients doRender
                WaitTimeout{} -> pure ()

isProcess :: SystemEvent -> Bool
isProcess = \case
    ProcessCreated{} -> True
    ProcessStopped{} -> True
    _ -> False
