module Control.Loger where

import           Universum

type Loger = Text -> IO ()

logToConsole :: Loger
logToConsole = putTextLn

logOff :: Loger
logOff _ = pure ()