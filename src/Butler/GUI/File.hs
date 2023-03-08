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
        with div_ [class_ "cursor-pointer", wsSend, wid_ wid "file-open", encodeVal ["fp" .= name]] do
            with i_ [class_ ("mx-2 my-3 w-6 " <> icon)] do
                toHtml name
