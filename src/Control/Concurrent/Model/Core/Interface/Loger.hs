module Control.Concurrent.Model.Core.Interface.Loger where

import           Control.Concurrent.Prelude
import           Control.Concurrent.Model.Core.Data.TextId

-- | https://stackoverflow.com/a/2031209
data LogLevel = Trace | Debug | Info | Warn | Error | Fatal
    deriving (Eq, Ord, Enum, Show)

type Loger = LogLevel -> Text -> IO ()

class Monad m => Logers m where
    toLog    :: LogLevel -> Text -> m ()
    getLoger :: m Loger 

addTagToLoger :: Loger -> Text -> TextId -> IO Loger
addTagToLoger logerAction tag textId =
    pure $ \logLevel txt -> logerAction logLevel (tag <> "\t" <> describe textId <> " " <> txt)
