module Butler.App.FileViewer where

import Butler
import Butler.App
import Butler.Service.FileService
import Data.ByteString qualified as BS

fileViewerApp :: App
fileViewerApp =
    (defaultApp "file-viewer" startFileViewerApp)
        { tags = fromList ["System"]
        , description = "View file content"
        , acceptFiles = Just BinaryContent
        }

data FileContent = FileContent File FileLoc ByteString

readFileContent :: Directory -> File -> ProcessIO FileContent
readFileContent dir file = FileContent file (getFileLoc dir (Just file)) . BS.take 4096 <$> readFileBS dir file

startFileViewerApp :: AppContext -> ProcessIO ()
startFileViewerApp ctx = do
    (currentFile, memFile) <- newProcessMemory (from $ withWID ctx.wid "fv") (pure mempty)
    rootDir <- getVolumeDirectory ctx.shared Nothing
    tmFile <-
        atomically (resolveFileLoc rootDir currentFile) >>= \case
            Just (dir, Just file) -> newTVarIO . Just =<< readFileContent dir file
            _ -> newTVarIO Nothing
    let
        launchers :: File -> FileLoc -> HtmlT STM ()
        launchers file fileLoc = do
            let contentType = Just $ fileContentType file
            let preferedApps = filter (\app -> contentType == app.acceptFiles) (appSetApps ctx.shared.appSet)
                extraApps
                    | contentType == Just BinaryContent = []
                    | otherwise = filter (\app -> Just BinaryContent == app.acceptFiles) (appSetApps ctx.shared.appSet)
            forM_ (preferedApps <> extraApps) \app -> do
                let swapScript = startAppScript app ["wid" .= ctx.wid, "fp" .= fileLoc]
                with a_ [class_ "cursor-pointer mx-1", onclick_ swapScript] do
                    toHtml app.name
        renderFileContent :: FileContent -> HtmlT STM ()
        renderFileContent = \case
            FileContent file loc buf -> do
                div_ $ toHtml (file.name)
                size <- lift (readTVar file.size)
                div_ $ "Size: " >> toHtml (into @Text size) >> " bytes"
                div_ $ "Open with: " >> launchers file loc
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
