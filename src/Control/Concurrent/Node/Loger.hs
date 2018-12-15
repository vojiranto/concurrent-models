{-# Language TemplateHaskell  #-}
{-# Language QuasiQuotes      #-}
{-# Language FlexibleContexts #-}
module Control.Concurrent.Node.Loger
    ( LogMessage(..)
    , runLogerActor
    , startLogListening
    , stopLogListening
    , сonsoleLogOn
    , setLogLevel
    , loger
    ) where

import           Control.Concurrent.Prelude
import           Control.Concurrent.Model
import           Control.Concurrent.Service.Subscription

newtype SetLogLevel = SetLogLevel LogLevel
data LogMessage     = LogMessage LogLevel Text

makeAct "LogerActor" [[t|LogMessage|], [t|SetLogLevel|], [t|Subscription|], [t|Unsubscribe|]]

runLogerActor :: IO LogerActor
runLogerActor = runRole dummyLoger $ do
    subscribers <- subscriptioService
    logLevelRef <- liftIO $ newIORef Warn
    math $ \(SetLogLevel logLever) -> (writeIORef logLevelRef logLever :: IO ())
    math $ \(LogMessage logLevel text) -> do
        counLevel <- readIORef logLevelRef
        when (logLevel >= counLevel) $ multicast subscribers $
            LogMessage logLevel text

startLogListening :: (HaveTextId a, Listener a LogMessage) => LogerActor -> a -> IO ()
startLogListening = $(subscribe [t|LogMessage|])

stopLogListening :: (HaveTextId a, Listener a LogMessage) => LogerActor -> a -> IO ()
stopLogListening = $(unsubscribe [t|LogMessage|])

сonsoleLogOn :: LogerActor -> IO (IO ())
сonsoleLogOn logActor = do
    consoleLoger <- runActor dummyLoger $
        math $ \(LogMessage logLevel text) ->
            ((putTextLn $ "[" <> show logLevel <> "]\t" <> text) :: IO ())
    startLogListening logActor consoleLoger
    pure $ do
        stopLogListening logActor consoleLoger
        stopRole consoleLoger

setLogLevel :: LogerActor -> LogLevel -> IO ()
setLogLevel logerActor = notify logerActor . SetLogLevel

loger :: LogerActor -> Loger
loger logerActor logLevel = notify logerActor . LogMessage logLevel