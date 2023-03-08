module Butler.Service.FileSystem (
    Entry (..),
    getFileSystem,
    fileSystemService,
    filesUploadButton,
)
where

import Butler
import Butler.Core.Dynamic

fileSystemService :: Service
fileSystemService = Service $ defaultApp "file-system" startFileSystem

data Entry
    = Directory RawFilePath [Entry]
    | File RawFilePath

newtype FileSystem = FileSystem (TVar Entry)

filesUploadButton :: Monad m => WinID -> HtmlT m ()
filesUploadButton wid = do
    with (input_ mempty) [wid_ wid inputID, multiple_ "", type_ "file", onchange_ changeScript]
  where
    inputID = "butlerfiles"
    changeScript = "butlerFilesChanges" <> showT (wid, inputID)

newFileSystem :: STM FileSystem
newFileSystem = FileSystem <$> newTVar (Directory "rootfs" [])

getFileSystem :: MonadUnliftIO m => AppContext -> m FileSystem
getFileSystem ctx =
    waitDynamic 150 ctx.shared.devices "file-system" >>= \case
        WaitCompleted sc -> pure sc
        WaitTimeout -> error "file-system service is not running"

startFileSystem :: AppContext -> ProcessIO ()
startFileSystem ctx = do
    fs <- atomically newFileSystem
    let mountUI = with div_ [wid_ ctx.wid "tray"] do
            script_ (fileSystemClient ctx.wid)
    withDynamic ctx.shared.devices "file-system" fs $ forever do
        atomically (readPipe ctx.pipe) >>= \case
            ae@(AppDisplay{}) -> sendHtmlOnConnect mountUI ae
            ev -> logDebug "Got ev" ["ev" .= ev]

fileSystemClient :: WinID -> Text
fileSystemClient wid =
    [raw|
function setupFileSystemClient(wid) {
  globalThis.butlerFilesChanges = (srcWID, inputID) => {
    const elt = document.getElementById(withWID(srcWID, inputID))
    console.log("Got: ", elt.files)
    elt.value = ''
  }
}
|]
        <> ("\nsetupFileSystemClient(" <> showT wid <> ");")
