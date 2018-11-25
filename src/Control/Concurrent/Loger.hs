module Control.Concurrent.Loger where

import           Universum
import           Data.TextId

type Loger = Text -> IO ()

-- | Alias for putTextLn.
logToConsole :: Loger
logToConsole = putTextLn

-- | Alias for (\\_ -> pure ()).
logOff :: Loger
logOff _ = pure ()

addTagToLoger :: (Text -> t) -> Text -> IO (Text -> t)
addTagToLoger logerAction tag = do
    textId   <- newTextId
    pure $ \txt -> logerAction $ tag <> " " <> "[" <> textId  <> "] " <> txt
