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
    emacsCmd xs = "tini" : "-s" : "nix" : "run" : "github:podenv/devenv/5acaa5446df29b08fb4dba008c314cb3b306d5ae#emacs-nox" : xs
