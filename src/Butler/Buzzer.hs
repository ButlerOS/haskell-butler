module Butler.Buzzer (Buzzer, newBuzzer) where

type Buzzer = Int -> IO ()

newBuzzer :: Buzzer
newBuzzer freq = do
    putStrLn $ "<<BEEP>> " <> show freq
