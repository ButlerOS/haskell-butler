module Butler where

import Butler.Clock
import Butler.OS

demo :: IO ()
demo =
    print =<< withButlerOS do
        sleep 1800_000
