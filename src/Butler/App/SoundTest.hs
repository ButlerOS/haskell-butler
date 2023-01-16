module Butler.App.SoundTest where

import Butler.Prelude

import Butler.Desktop
import Butler.GUI
import Butler.SoundBlaster
import Butler.Window

soundTestHtml :: WinID -> HtmlT STM ()
soundTestHtml wid = ul_ do
    with li_ [class_ "flex flex-row m-1"] do
        with span_ [class_ "grow"] mempty
        with span_ [class_ "px-2 py-1"] "Für Elise 8kHz"
        with button_ [id_ (withWID wid "sin-test"), wsSend, hxTrigger_ "click", class_ "bg-blue-500 hover:bg-blue-700 text-white font-bold p-1 rounded"] "play"

soundTestApp :: Desktop -> WinID -> ProcessIO GuiApp
soundTestApp desktop winID = do
    size <- newTVarIO (164, 164)
    newGuiAppWin winID "sound-test" (Just size) (const $ pure $ soundTestHtml winID) ["sin-test"] \app -> forever do
        ev <- atomically (readPipe app.events)
        logInfo "got ev" ["ev" .= ev]
        case withoutWID ev.trigger of
            "sin-test" -> playSample desktop.soundCard $ renderNotes furElise 4
            _ -> logError "unknown trigger" ["ev" .= ev]
