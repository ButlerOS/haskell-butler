module XStatic.Butler where

import XStatic
import XStatic.TH

logo :: XStaticFile
logo = $(embedXStaticFile "data/haskell-butler-logo.svg"){xfPath = "/favicon.ico"}
