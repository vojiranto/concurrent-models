{-# Language TemplateHaskell  #-}
{-# Language QuasiQuotes      #-}
{-# Language FlexibleContexts #-}
module Control.Concurrent.Loger
    ( logToConsole
    , logOff
    , LogMessage(..)
    , startLogListening
    , stopLogListening
    , сonsoleLogOn
    , setLogLevel
    , loger
    ) where

import           Control.Concurrent.Prelude
import           Control.Concurrent.Model
import           Control.Concurrent.Actor
import           Control.Concurrent.Service.Subscription
import           System.IO.Unsafe

-- | Alias for putTextLn.
logToConsole :: Loger
logToConsole logLevel msg = putTextLn $ "[" <> show logLevel <> "] " <> msg

-- | Alias for (\\_ -> pure ()).
logOff :: Loger
logOff _ _ = pure ()

newtype SetLogLevel = SetLogLevel LogLevel
data LogMessage = LogMessage LogLevel Text

logerActor :: Actor
{-# NOINLINE logerActor #-}
logerActor = unsafePerformIO $ runActor logOff $ do
    subscribers <- subscriptioService
    logLevelRef <- liftIO $ newIORef Trace
    math $ \(SetLogLevel logLever) -> (writeIORef logLevelRef logLever :: IO ())
    math $ \(LogMessage logLevel text) -> do
        counLevel <- readIORef logLevelRef
        when (logLevel >= counLevel) $ multicast subscribers $
            LogMessage logLevel text

startLogListening :: (HaveTextId a, Listener a LogMessage) => a -> IO ()
startLogListening = $(subscribe [t|LogMessage|]) logerActor

stopLogListening :: (HaveTextId a, Listener a LogMessage) => a -> IO ()
stopLogListening = unsubscribe (LogMessage Trace "") logerActor

сonsoleLogOn :: IO (IO ())
сonsoleLogOn = do
    consoleLoger <- runActor logOff $
        math $ \(LogMessage logLevel text) ->
            ((putTextLn $ "[" <> show logLevel <> "] " <> text) :: IO ())
    startLogListening consoleLoger
    pure $ do
        stopLogListening consoleLoger
        stopRole consoleLoger

setLogLevel :: LogLevel -> IO ()
setLogLevel logLevel = notify logerActor $ SetLogLevel logLevel

loger :: Loger
loger logLevel text = notify logerActor $ LogMessage logLevel text