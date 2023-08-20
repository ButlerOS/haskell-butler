module Butler.Core.Buzzer (Buzzer, newBuzzer) where

import Prelude

type Buzzer = Int -> IO ()

-- TODO: buzz to the display users.
newBuzzer :: Buzzer
newBuzzer freq = do
    putStrLn $ "<<BEEP>> " <> show freq
