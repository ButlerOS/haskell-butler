module Butler.Service.FileService (
    VolumeName,
    getVolumeDirectory,
    fileService,
    filesUploadButton,
)
where

import Butler
import Butler.Core.Dynamic
import Butler.Core.NatMap qualified as NM

import Butler.Frame
import Data.ByteString qualified as BS

fileService :: Service
fileService = Service $ defaultApp "file-service" startFileService

filesUploadButton :: Monad m => WinID -> FileLoc -> HtmlT m ()
filesUploadButton wid dname = do
    with (input_ mempty) [wid_ wid inputID, multiple_ "", type_ "file", onchange_ changeScript]
  where
    inputID = "butlerfiles"
    changeScript = "butlerFilesChanges" <> showT (wid, dname, inputID)

-- | 'VolumeName' is the name of a root directory, e.g. "Documents" or "Desktop"
newtype VolumeName = VolumeName FileName
    deriving newtype (IsString)

-- | 'getVolumeDirectory' enables app to get access to the File API.
getVolumeDirectory :: AppSharedContext -> Maybe VolumeName -> ProcessIO Directory
getVolumeDirectory shared mVolume =
    waitDynamic 150 shared.dynamics "file-service" >>= \case
        WaitCompleted sc -> getVolumeDir sc
        WaitTimeout -> error "file-system service is not running"
  where
    getVolumeDir :: Directory -> ProcessIO Directory
    getVolumeDir dir = case mVolume of
        Nothing -> pure dir
        Just (VolumeName name) -> do
            mDir <- atomically do
                lookupChild dir name >>= \case
                    Just (Directory volumeDir) -> pure (Just volumeDir)
                    Just (File _) -> pure Nothing
                    Nothing -> newDirectory dir name
            case mDir of
                Nothing -> do
                    logError "Volume is not a directory" ["name" .= name]
                    pure dir
                Just volumeDir -> pure volumeDir

-- | FileUploadRequest is a single file description that will be uploaded by the client browser.
data FileUploadRequest = FileUploadRequest
    { name :: FileName
    , size :: Natural
    }
    deriving (Generic, FromJSON, ToJSON)

-- | UploadRequest is what the client browser send when the <input type=file> changes.
data UploadRequest = UploadRequest
    { dirname :: FileLoc
    -- ^ The target directory name
    , uid :: Natural
    -- ^ The client Upload Request Id
    , wid :: WinID
    -- ^ The target window
    , files :: [FileUploadRequest]
    -- ^ The list of files to be uploaded
    }
    deriving (Generic, FromJSON, ToJSON)

-- | For a given UploadRequest, the service creates a PendingUpload for each files.
data PendingUpload = PendingUpload
    { pfile :: PartialFile
    , size :: Natural
    , current :: TVar Natural
    }

appendPendingUpload :: PendingUpload -> ByteString -> ProcessIO Natural
appendPendingUpload pendingUpload buf = do
    appendPartialFile pendingUpload.pfile buf
    atomically $
        stateTVar pendingUpload.current $
            \current -> let new = current + unsafeFrom (BS.length buf) in (new, new)

startFileService :: AppContext -> ProcessIO ()
startFileService ctx = do
    let mountUI = with div_ [wid_ ctx.wid "tray"] do
            script_ (fileSystemClient ctx.wid)
    rootDir <- do
        dpath <- getPath "rootfs/"
        fromMaybe (error "bad rootfs?") <$> readRootDirectory dpath

    uploads <- atomically (NM.newNatMap @PendingUpload)

    let handleFileUploadRequest :: Directory -> FileUploadRequest -> ProcessIO Natural
        handleFileUploadRequest dir fileUploadRequest = do
            pfile <- createPartialFile dir fileUploadRequest.name
            pendingUpload <- PendingUpload pfile fileUploadRequest.size <$> newTVarIO 0
            -- Create the Pending Upload ID (PUI)
            atomically $ NM.add uploads pendingUpload

        handleUploadRequest :: DisplayClient -> UploadRequest -> ProcessIO ()
        handleUploadRequest client uploadRequest = do
            logInfo "Handling upload request" ["req" .= uploadRequest]
            atomically (resolveFileLoc rootDir uploadRequest.dirname) >>= \case
                Just (dir, Nothing) -> do
                    -- Got a valid UploadRequest, setup the PendingUpload file and returns their id (PUI)
                    puis <- traverse (handleFileUploadRequest dir) uploadRequest.files
                    let resp = encodeMessage uploadRequest.uid $ encodeJSON puis
                    atomically $ sendBinary client (encodeMessageL ctx.wid resp)
                _ -> logError "Unknown dir" ["req" .= uploadRequest]

        handlePendingUpload :: Natural -> ByteString -> ProcessIO ()
        handlePendingUpload pui buf =
            atomically (NM.lookup uploads pui) >>= \case
                Just pendingUpload -> do
                    -- Got a valid data buffer
                    current <- appendPendingUpload pendingUpload buf
                    when (current > pendingUpload.size) do
                        logError "Received too much!" ["got" .= current, "expected" .= pendingUpload.size]
                    when (current >= pendingUpload.size) do
                        void $ finalizePartialFile pendingUpload.pfile (from current)
                        atomically $ NM.delete uploads pui
                Nothing -> logError "Unknown upload req" ["ev" .= pui]

    withDynamic ctx.shared.dynamics "file-service" rootDir $ forever do
        atomically (readPipe ctx.pipe) >>= \case
            ae@(AppDisplay{}) -> sendHtmlOnConnect mountUI ae
            AppTrigger ev -> case ev.trigger of
                "upload-request" -> case ev.body ^? key "request" . _JSON of
                    Just uploadRequest -> handleUploadRequest ev.client uploadRequest
                    Nothing -> logError "Missing request" ["ev" .= ev]
                _ -> logError "Unknown trigger" ["ev" .= ev]
            AppData ev -> case decodeMessage ev.buffer of
                Just (pui, buf) -> handlePendingUpload pui buf
                Nothing -> logError "Unknown data event" ["ev" .= ev]
            ev -> logDebug "Got ev" ["ev" .= ev]

fileSystemClient :: WinID -> Text
fileSystemClient wid =
    [raw|
function setupFileSystemClient(wid) {
  const uploadRequests = {}
  const newUploadRequest = (wid, dirname) => {
    const uid = Object.keys(uploadRequests).length
    uploadRequests[uid] = {uid, wid, dirname, files: []}
    return uploadRequests[uid]
  }

  globalThis.butlerFilesChanges = (srcWID, path, inputID) => {
    const elt = document.getElementById(withWID(srcWID, inputID))
    const request = newUploadRequest(srcWID, path)
    for (const file of elt.files) {
      request.files.push({name: file.name, size: file.size})
    }
    console.log("Sending upload request", request)
    sendTrigger(wid, "upload-request", {request})
    request.elt = elt
  }

  butlerDataHandlers[wid] = rawBuf => decodeDataMessage(rawBuf, (uid, buf) => {
    // We got the Pending Upload ID (puis) and the upload request.
    const puis = decodeJSON(buf)
    const request = uploadRequests[uid]

    // Setup the upload promise...
    const promises = []
    for (const file of request.elt.files) {
      const fileUploadID = puis.shift()
      if (fileUploadID === undefined) {
        console.error("Unknown file id", resp, request.elt.files)
        throw "abort!"
      }
      console.log("Processing", fileUploadID, file)

      promises.push(file.arrayBuffer().then(buf => {
        let fileOffset = 0
        let arr = new Uint8Array(buf)
        while (true) {
          console.log("Reading", fileOffset, fileOffset + 4096)
          const chunk = arr.slice(fileOffset, fileOffset + 4096)
          if (chunk.length == 0) {
            break
          }
          sendBinaryMessage2(wid, fileUploadID, arr)
          fileOffset += chunk.length
         }
      }))
    }

    // Wait for all uploads.
    Promise.all(promises).then(() => {
      delete uploadRequests[uid]
      request.elt.value = ''
      console.log("Done with upload request", uid)
    })
  })
}
|]
        <> ("\nsetupFileSystemClient(" <> showT wid <> ");")
