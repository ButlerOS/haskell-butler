module Butler.App.Terminal.Emacs (emacsApp) where

import Butler
import Butler.App.Terminal (TermApp (..), startTermApp)
import Butler.UnixShell

emacsApp :: Isolation -> App
emacsApp isolation =
    (defaultApp "emacs" (startTermApp isolation (const . const emacsTermApp)))
        { tags = fromList ["Development"]
        , description = "IDE"
        }

emacsTermApp :: TermApp
emacsTermApp =
    TermApp
        { backgroundCommand = Nothing
        , attachCommand = emacsCmd []
        }
  where
    emacsCmd xs = "tini" : "nix" : "run" : "github:podenv/devenv#emacs-nox" : xs
