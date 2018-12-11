{-# Language TemplateHaskell  #-}
{-# Language QuasiQuotes      #-}
{-# Language FlexibleContexts #-}
module Control.Concurrent.Node.Console where

import           Control.Concurrent.Prelude
import           Control.Concurrent.Service.Subscription
import           Control.Concurrent.Model
import           Control.Concurrent.Service.Stream
import qualified Data.ByteString as B

runConsoleWorker :: Loger -> IO Stream
runConsoleWorker loger = runFsm loger Opened $ do
    subscribers <- subscriptioService
    myRef <- this
    liftIO $ readerWorker B.getLine myRef
    math $ \(Inbox message) ->
        multicast subscribers $ Message (getTextId myRef) message
    math $ \message -> catchAny
        (putTextLn message)
        (\_ -> notify myRef CommandClose)
    closeLogic subscribers myRef $ pure ()

readerWorker
    :: (Listener a Inbox, Listener a ByteString)
    => IO ByteString -> a -> IO ()
readerWorker readerAction myRef =
    void $ forkIO $ forever $ do
        rawData <- readerAction
        notify myRef (Inbox rawData)