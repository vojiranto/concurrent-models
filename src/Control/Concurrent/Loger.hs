{-# Language TemplateHaskell  #-}
{-# Language QuasiQuotes      #-}
{-# Language FlexibleContexts #-}
module Control.Concurrent.Loger
    ( dummyLoger
    , LogMessage(..)
    , startLogListening
    , stopLogListening
    , сonsoleLogOn
    , setLogLevel
    , loger
    ) where

import           Control.Concurrent.Prelude
import           Control.Concurrent.Model
import           Control.Concurrent.Service.Subscription
import           System.IO.Unsafe

dummyLoger :: Loger
dummyLoger _ _ = pure ()

newtype SetLogLevel = SetLogLevel LogLevel
data LogMessage     = LogMessage LogLevel Text

logerActor :: Actor
{-# NOINLINE logerActor #-}
logerActor = unsafePerformIO $ runActor dummyLoger $ do
    subscribers <- subscriptioService
    logLevelRef <- liftIO $ newIORef Warn
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
    consoleLoger <- runActor dummyLoger $
        math $ \(LogMessage logLevel text) ->
            ((putTextLn $ "[" <> show logLevel <> "]\t" <> text) :: IO ())
    startLogListening consoleLoger
    pure $ do
        stopLogListening consoleLoger
        stopRole consoleLoger

setLogLevel :: LogLevel -> IO ()
setLogLevel logLevel = notify logerActor $ SetLogLevel logLevel

loger :: Loger
loger logLevel text = notify logerActor $ LogMessage logLevel text