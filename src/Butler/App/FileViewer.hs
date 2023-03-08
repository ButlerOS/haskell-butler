module Butler.App.FileViewer where

import Butler
import Butler.Service.FileService

fileViewerApp :: App
fileViewerApp =
    (defaultApp "file-viewer" startFileViewerApp)
        { tags = fromList ["System"]
        , description = "View file content"
        }

data FileContent = FileContent File ByteString

readFileContent :: Directory -> File -> ProcessIO FileContent
readFileContent dir file = FileContent file <$> readFileBS dir file

startFileViewerApp :: AppContext -> ProcessIO ()
startFileViewerApp ctx = do
    (currentFile, memFile) <- newProcessMemory (from $ withWID ctx.wid "fv") (pure mempty)
    rootDir <- getVolumeDirectory ctx.shared Nothing
    tmFile <-
        atomically (resolveFileLoc rootDir currentFile) >>= \case
            Just (dir, Just file) -> newTVarIO . Just =<< readFileContent dir file
            _ -> newTVarIO Nothing
    let
        renderFileContent :: FileContent -> HtmlT STM ()
        renderFileContent = \case
            FileContent file buf -> do
                div_ $ toHtml (file.name)
                size <- lift (readTVar file.size)
                div_ $ "Size: " >> toHtml (into @Text size) >> " bytes"
                div_ do
                    pre_ (toHtml (decodeUtf8With lenientDecode buf))

        mountUI =
            with div_ [wid_ ctx.wid "w", class_ "grid grid-cols-1 divide-y"] do
                lift (readTVar tmFile) >>= \case
                    Nothing -> pure ()
                    Just file -> renderFileContent file
    forever do
        atomically (readPipe ctx.pipe) >>= \case
            ae@AppDisplay{} -> sendHtmlOnConnect mountUI ae
            AppFile dir (Just file) -> do
                fileContent <- readFileContent dir file
                atomically do
                    writeTVar tmFile (Just fileContent)
                    modifyMemoryVar memFile (const $ getFileLoc dir (Just file))
                sendsHtml ctx.clients (renderFileContent fileContent)
            _ -> pure ()
