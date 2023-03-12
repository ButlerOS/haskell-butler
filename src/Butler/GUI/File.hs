module Butler.GUI.File where

import Butler

renderFileIcon :: Entry -> (Text, FileName)
renderFileIcon = \case
    Directory dir -> ("ri-folder-line", from dir.path)
    File file -> ("ri-file-line", from file.name)

renderFileIcons :: WinID -> Directory -> HtmlT STM ()
renderFileIcons wid rootDir = with div_ [wid_ wid "file-list"] do
    childs <- lift (readTVar rootDir.childs)
    forM_ childs \child -> do
        let (icon, name) = renderFileIcon child
            (loc, program) = case child of
                Directory dir -> (getFileLoc dir Nothing, ProgramName "file-manager")
                File file -> (getFileLoc rootDir (Just file), ProgramName "file-viewer")
        withTrigger "click" (WinID 0) "start-app" ["name" .= program, "fp" .= loc] div_ [class_ "cursor-pointer"] do
            with i_ [class_ ("mx-2 my-3 w-6 " <> icon)] do
                toHtml name
