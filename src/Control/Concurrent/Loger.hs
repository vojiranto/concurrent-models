module Control.Concurrent.Loger where

import           Control.Concurrent.Prelude
import           Control.Concurrent.Model.Data.TextId

-- https://stackoverflow.com/a/2031209
data LogLevel = Trace | Debug | Info | Warn | Error | Fatal
    deriving (Eq, Enum, Show)

type Loger = LogLevel -> Text -> IO ()

-- | Alias for putTextLn.
logToConsole :: Loger
logToConsole logLevel msg = putTextLn $ "[" <> show logLevel <> "] " <> msg

-- | Alias for (\\_ -> pure ()).
logOff :: Loger
logOff _ _ = pure ()

addTagToLoger :: Loger -> Text -> TextId -> IO Loger
addTagToLoger logerAction tag textId =
    pure $ \logLevel txt -> logerAction logLevel (tag <> " " <> describe textId <> " " <> txt)
