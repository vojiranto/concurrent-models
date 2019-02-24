{-# Language TemplateHaskell  #-}
{-# Language QuasiQuotes      #-}
{-# Language FlexibleContexts #-}
module Control.Concurrent.Node.Console where

import           Control.Concurrent.Prelude
import           Control.Concurrent.Service.Subscription
import           Control.Concurrent.Model
import           Control.Concurrent.Service.ByteStream
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8

runConsoleWorker :: Loger -> IO ByteStream
runConsoleWorker loger = runFsm loger Opened $ do
    subscribers <- subscriptionService
    myRef <- this
    liftIO $ readerWorker B.getLine myRef
    math $ \(Inbox message) ->
        multicast subscribers $ Message (getTextId myRef) message
    math $ \(message :: B.ByteString) -> catchAny
        (B8.putStrLn message)
        (\_ -> notify myRef CommandClose)
    streemCloseLogic subscribers myRef $ pure ()

readerWorker
    :: (Listener a Inbox, Listener a ByteString)
    => IO ByteString -> a -> IO ()
readerWorker readerAction myRef =
    void $ forkIO $ forever $ do
        rawData <- readerAction
        notify myRef (Inbox rawData)