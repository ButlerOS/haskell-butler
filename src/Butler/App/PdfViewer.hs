module Butler.App.PdfViewer (pdfViewerApp) where

import Butler
import Butler.Frame
import Butler.Service.FileService

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL

import XStatic.PdfJS qualified as XStatic

pdfViewerApp :: App
pdfViewerApp =
    (defaultApp "pdfViewer" startPdfViewer)
        { tags = fromList ["Utility"]
        , description = "A pdfViewer app"
        , xfiles = [XStatic.pdfJs]
        , extraXfiles = [XStatic.pdfJsWorker]
        , acceptFiles = Just PdfContent
        }

data PdfState = PdfState
    { dir :: Directory
    , file :: File
    , page :: Int
    }
    deriving (Generic)

newPdfState :: Directory -> File -> PdfState
newPdfState dir file = PdfState dir file 1

startPdfViewer :: AppContext -> ProcessIO ()
startPdfViewer ctx = do
    (currentFile, memFile) <- newProcessMemory (from $ withWID ctx.wid "noter-file") (pure mempty)
    rootDir <- getVolumeDirectory ctx.shared Nothing
    tmState <-
        atomically (resolveFileLoc rootDir currentFile) >>= \case
            Just (dir, Just file) -> newTVarIO $ Just (newPdfState dir file)
            _ -> newTVarIO Nothing
    -- UI
    let mountUI :: HtmlT STM ()
        mountUI = with div_ [wid_ ctx.wid "w", class_ "flex flex-col"] do
            with div_ [class_ "flex flex-row gap-2"] do
                withTrigger_ "click" ctx.wid "prev-page" button_ [class_ btnBlueClass] "<"
                withTrigger_ "click" ctx.wid "next-page" button_ [class_ btnBlueClass] ">"
                with div_ [wid_ ctx.wid "page-current"] "?"
                div_ "/"
                with div_ [wid_ ctx.wid "page-count"] "?"
            with canvas_ [id_ "the-canvas"] mempty
            script_ $ pdfClient ctx.wid

        pageMessage page = encodeMessage (from ctx.wid) (BSL.pack [1, unsafeFrom page])

        updatePage :: (Int -> Int) -> ProcessIO ()
        updatePage dir = do
            mNewPage <- atomically do
                readTVar tmState >>= \case
                    Nothing -> pure Nothing
                    Just state ->
                        Just <$> do
                            let newPage = max 1 (dir state.page)
                            writeTVar tmState (Just $ state & #page .~ newPage)
                            pure newPage
            forM_ mNewPage \page -> do
                sendsBinary ctx.shared.clients (pageMessage page)

    -- Handle events
    forever do
        atomically (readPipe ctx.pipe) >>= \case
            AppDisplay (UserJoined client) -> do
                atomically $ sendHtml client mountUI
                readTVarIO tmState >>= \case
                    Nothing -> pure ()
                    Just pdfState -> do
                        buf <- readFileBS pdfState.dir pdfState.file
                        logDebug "Sending" ["file" .= pdfState.file, "size" .= BS.length buf]
                        atomically $ sendBinary client (encodeMessage (from ctx.wid) (BSL.cons 0 $ from buf))
                        atomically $ sendBinary client (pageMessage pdfState.page)
            AppTrigger ev -> case ev.trigger of
                "prev-page" -> updatePage (\x -> x - 1)
                "next-page" -> updatePage (+ 1)
                _ -> logError "Unknown trigger" ["ev" .= ev]
            AppFile dir (Just file) -> atomically do
                modifyMemoryVar memFile (const $ getFileLoc dir (Just file))
                writeTVar tmState (Just $ newPdfState dir file)
            ev -> logError "Unknown ev" ["ev" .= ev]

pdfClient :: AppID -> Text
pdfClient wid =
    [raw|
function setupPdfClient(wid) {
  const elt = document.getElementById(withWID(wid, "w"))
  const curElt = document.getElementById(withWID(wid, "page-current"))
  const countElt = document.getElementById(withWID(wid, "page-count"))

  // Loaded via <script> tag, create shortcut to access PDF.js exports.
  const pdfjsLib = window['pdfjs-dist/build/pdf']

  // The workerSrc property shall be specified.
  pdfjsLib.GlobalWorkerOptions.workerSrc = '/xstatic/pdf.worker.min.js'

  var pdf = null
  var nextPage = 1
  const renderPage = pageNumber => {
    curElt.textContent = pageNumber
    // Fetch the first page
    pdf.getPage(pageNumber).then(function(page) {
      console.log('Page loaded');

      const viewport = page.getViewport({scale: 2});

      // Prepare canvas using PDF page dimensions
      const canvas = document.getElementById('the-canvas');
      const context = canvas.getContext('2d');

      canvas.width = viewport.width;
      canvas.height = viewport.height;
      const renderScale = 2 * canvas.width / viewport.width
      // Render PDF page into canvas context
      const renderContext = {
        canvasContext: context,
        viewport: viewport.clone({ scale: renderScale })
      };
      const renderTask = page.render(renderContext);
      renderTask.promise.then(function () {
        console.log('Page rendered');
      });
    });
  }

  butlerDataHandlers[wid] = buf => {
    const ev = buf[0];
    const data = buf.slice(1)
    if (ev == 0) {
      console.log("Got document data:", buf.length)
      const loadingTask = pdfjsLib.getDocument({data: buf})
      loadingTask.promise.then(pdf_ => {
        console.log('PDF loaded');
        pdf = pdf_;
        countElt.textContent = pdf.numPages
        renderPage(nextPage)
      }, reason => {
        // PDF loading error
        console.error(reason);
      })
    } else if (ev == 1) {
      console.log("Got page number:", data)
      const page = data[0]
      if (pdf === null) {
        nextPage = page
      } else {
        renderPage(page)
      }
    }
  }
}
|]
        <> ("\nsetupPdfClient(" <> showT wid <> ");")
