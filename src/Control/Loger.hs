module Control.Loger where

import           Universum

type Loger = Text -> IO ()

-- | Alias for putTextLn.
logToConsole :: Loger
logToConsole = putTextLn

-- | Alias for (\\_ -> pure ()).
logOff :: Loger
logOff _ = pure ()