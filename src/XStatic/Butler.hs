module XStatic.Butler where

import XStatic
import XStatic.Htmx qualified as XStatic
import XStatic.TH
import XStatic.Tailwind qualified as XStatic

logo :: XStaticFile
logo = $(embedXStaticFile "data/haskell-butler-logo.svg"){xfPath = "/favicon.ico"}

-- | ws patch version 1
butlerWS :: XStaticFile
butlerWS = $(embedXStaticFile "data/ws.js")

defaultXFiles :: [XStaticFile]
defaultXFiles = [XStatic.htmx, butlerWS, XStatic.tailwind]
