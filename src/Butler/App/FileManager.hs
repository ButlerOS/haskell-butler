module Butler.App.FileManager where

import Data.Set qualified as Set

import Butler
import Butler.App
import Butler.GUI.File
import Butler.Service.FileService

fileManagerApp :: App
fileManagerApp =
    (defaultApp "file-manager" startFileManagerApp)
        { tags = fromList ["System"]
        , description = "Manage files"
        , acceptFiles = Just ArchiveContent
        }

startFileManagerApp :: AppContext -> ProcessIO ()
startFileManagerApp ctx = do
    (currentDirectory, memDirectory) <- newProcessMemory (from $ withWID ctx.wid "fm") (pure mempty)
    rootDir <- getVolumeDirectory ctx.shared Nothing
    tDir <- atomically do
        resolveFileLoc rootDir currentDirectory >>= \case
            Just (prevDir, Nothing) -> newTVar prevDir
            _ -> newTVar rootDir

    -- The filename currently being edited
    tEditFile <- newTVarIO mempty
    -- The list of selected file
    tSelected <- newTVarIO (mempty :: Set FileName)
    -- The move destination folder
    tMoveTarget <- newTVarIO Nothing

    let
        getParent :: Directory -> [Directory]
        getParent dir = dir : maybe [] getParent dir.parent

        renderCrum :: Directory -> HtmlT STM ()
        renderCrum dir =
            with a_ [class_ "cursor-pointer", wsSend, wid_ ctx.wid "back-crum", encodeVal ["fp" .= getFileLoc dir Nothing]] do
                toHtml $ decodeUtf8 $ baseName dir.path

        renderCrums :: [Directory] -> HtmlT STM ()
        renderCrums [] = pure ()
        renderCrums [cur] = renderCrum cur
        renderCrums (cur : rest) = renderCrum cur >> " ▶ " >> renderCrums rest

        renderFileList :: Set FileName -> Directory -> HtmlT STM ()
        renderFileList selected dir = do
            -- TODO: disable triggers when moving the files...
            childs <- lift (readTVar dir.childs)
            editFile <- lift (readTVar tEditFile)
            forM_ childs \child -> do
                let (icon, name) = renderFileIcon child
                with div_ [class_ "flex"] do
                    let fn = into @FileName child
                        checkbox = butlerCheckbox ctx.wid "select-file" ["fp" .= fn] (fn `Set.member` selected) Nothing
                    input_ $ class_ "relative top-1 mx-1" : checkbox

                    -- ✏ edit button
                    when (editFile == mempty) do
                        withTrigger "click" ctx.wid "start-rename" ["fp" .= fn] i_ [class_ "ri-edit-box-line"] mempty
                    -- cancel edit button
                    when (editFile == fn) do
                        withTrigger_ "click" ctx.wid "stop-rename" i_ [class_ "ri-edit-circle-line"] mempty

                    with i_ [class_ ("flex-grow " <> icon)] do
                        if editFile == fn
                            then -- the edit input box

                                let attrs = [class_ "form-control", type_ "text", wsSend, name_ "new-name", value_ (into @Text fn)]
                                 in withTrigger "" ctx.wid "file-rename" ["fp" .= fn] (input_ attrs) []
                            else -- the file text

                            case child of
                                Directory{} -> withTrigger "click" ctx.wid "chdir" ["fp" .= fn] span_ [class_ "cursor-pointer"] (toHtml name)
                                File file ->
                                    let vals = ["name" .= ProgramName "file-viewer", "fp" .= getFileLoc dir (Just file)]
                                     in withTrigger "click" (WinID 0) "start-app" vals span_ [class_ "cursor-pointer"] (toHtml name)

        renderMoveTarget :: Directory -> HtmlT STM ()
        renderMoveTarget dir = do
            childs <- lift (readTVar dir.childs)
            ul_ do
                forM_ childs \case
                    File{} -> pure ()
                    Directory subDir -> do
                        let fn = into @FileName subDir.path
                        withTrigger "click" ctx.wid "select-target" ["fp" .= fn] li_ [class_ "cursor-pointer"] (toHtml fn)

        mountUI = do
            dir <- lift (readTVar tDir)
            selected <- lift (readTVar tSelected)
            mMoveTarget <- lift (readTVar tMoveTarget)
            with div_ [wid_ ctx.wid "w", class_ "grid grid-cols-1 divide-y"] do
                div_ do
                    renderCrums (reverse $ getParent dir)
                with div_ [class_ "grid grid-cols-2"] do
                    div_ do
                        renderFileList selected dir
                    div_ do
                        forM_ mMoveTarget renderMoveTarget

                unless (selected == mempty) do
                    with div_ [class_ "flex gap-2"] do
                        case mMoveTarget of
                            Nothing -> withTrigger_ "click" ctx.wid "start-move" span_ [class_ btnBlueClass] "Move Files"
                            Just _ -> withTrigger_ "click" ctx.wid "stop-move" span_ [class_ btnBlueClass] "Cancel move"
                        withTrigger_ "click" ctx.wid "files-delete" span_ [class_ btnRedClass] "Delete Files"
                        case mMoveTarget of
                            Just moveTarget | moveTarget.path /= dir.path -> do
                                with span_ [class_ "flex-grow"] mempty
                                withTrigger_ "click" ctx.wid "files-move" span_ [class_ btnGreenClass] (toHtml $ "Move files to '/" <> into @Text (getFileLoc moveTarget Nothing) <> "'")
                            _ -> pure ()
                div_ do
                    filesUploadButton ctx.wid (getFileLoc dir Nothing)
                div_ do
                    input_
                        [ class_ "form-control w-full"
                        , type_ "text"
                        , name_ "fp"
                        , wid_ ctx.wid "mkdir"
                        , placeholder_ "Create directory"
                        , wsSend
                        ]

        chDir dir = do
            writeTVar tDir dir
            modifyMemoryVar memDirectory (const $ getFileLoc dir Nothing)

        openDir dir name = atomically do
            lookupChild dir name >>= \case
                Just (Directory newDir) -> chDir newDir
                _ -> pure ()

        openFileLoc _dir name = atomically do
            resolveFileLoc rootDir (from name) >>= \case
                Just (newDir, Nothing) -> chDir newDir
                _ -> pure ()

        tryDeleteFile dir name = do
            atomically (lookupChild dir name) >>= \case
                Just (Directory target) -> deleteDirectory dir target
                Just (File target) -> deleteFile dir target
                Nothing -> pure ()

        deleteFiles = do
            (dir, files) <- atomically do
                dir <- readTVar tDir
                selected <- readTVar tSelected
                writeTVar tSelected mempty
                pure (dir, Set.toList selected)
            traverse_ (tryDeleteFile dir) files

        tryMoveEntry src dst name = do
            atomically (lookupChild src name) >>= \case
                Just entry -> moveEntry src dst entry
                Nothing -> pure ()

        moveFiles = do
            mParams <- atomically do
                dir <- readTVar tDir
                selected <- readTVar tSelected
                writeTVar tSelected mempty
                readTVar tMoveTarget >>= \case
                    Nothing -> pure Nothing
                    Just moveTarget -> do
                        writeTVar tMoveTarget Nothing
                        pure (Just (dir, moveTarget, selected))
            forM_ mParams \(src, dst, selected) -> do
                traverse_ (tryMoveEntry src dst) selected

        startRename _dir name = atomically (writeTVar tEditFile name)

        doRename Nothing _ _ = atomically (writeTVar tEditFile mempty)
        doRename (Just newName) dir name = do
            atomically (lookupChild dir name) >>= \case
                Just e -> renameEntry dir e newName
                Nothing -> pure ()
            atomically (writeTVar tEditFile mempty)

        selectMoveTarget _dir name = atomically do
            readTVar tMoveTarget >>= \case
                Nothing -> pure ()
                Just moveTarget ->
                    lookupChild moveTarget name >>= \case
                        Just (Directory newTarget) -> writeTVar tMoveTarget (Just newTarget)
                        _ -> pure ()

        toggleSelection _dir name = atomically $ modifyTVar' tSelected \selected ->
            if name `Set.member` selected
                then Set.delete name selected
                else Set.insert name selected

    spawnThread_ $ renderOnChange mountUI \newHtml -> do
        logDebug "Updating file-manager ui" []
        sendsHtml ctx.shared.clients newHtml

    let
        withFileName :: GuiEvent -> (Directory -> FileName -> ProcessIO ()) -> ProcessIO ()
        withFileName ev cb = case ev.body ^? key "fp" . _JSON of
            Just fp -> do
                dir <- readTVarIO tDir
                cb dir fp
            Nothing -> logError "Missing fp" ["ev" .= ev]

    forever do
        atomically (readPipe ctx.pipe) >>= \case
            ae@AppDisplay{} -> sendHtmlOnConnect mountUI ae
            AppFile dir _ -> atomically $ writeTVar tDir dir
            AppTrigger ev -> do
                case ev.trigger of
                    -- Navigation events
                    "chdir" -> withFileName ev openDir
                    "back-crum" -> withFileName ev openFileLoc
                    -- Selection events
                    "select-file" -> withFileName ev toggleSelection
                    "files-delete" -> deleteFiles
                    -- Moving events
                    "start-move" -> atomically (writeTVar tMoveTarget (Just rootDir))
                    "select-target" -> withFileName ev selectMoveTarget
                    "stop-move" -> atomically (writeTVar tMoveTarget Nothing)
                    "files-move" -> moveFiles
                    -- Renaming events
                    "start-rename" -> withFileName ev startRename
                    "stop-rename" -> atomically (writeTVar tEditFile mempty)
                    "file-rename" -> withFileName ev (doRename $ ev.body ^? key "new-name" . _JSON)
                    -- Create a new directory
                    "mkdir" -> withFileName ev (\dir -> void . atomically . newDirectory dir)
                    _ -> logError "Unknown ev" ["ev" .= ev]
            _ -> pure ()
