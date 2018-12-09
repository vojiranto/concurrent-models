{-# Language TemplateHaskell  #-}
{-# Language QuasiQuotes      #-}
module Tcp where

import           Universum

import           Control.Concurrent.Loger
import           Control.Concurrent.Model
import           Control.Concurrent.Flag
import           Control.Concurrent.Service.StreamController
--import           Control.Concurrent.Service.Subscription

tcpExample :: IO ()
tcpExample = do
    success <- newFlag
    let pDescribe = packegeDescribe 50 id Just
    controller <- runActor loger $ do
        toLog Info "Start of central controller"
        math $ \(Message _ (_ :: ByteString)) -> liftFlag success

    void $ makeTcpServer loger controller 5000 pDescribe
    output <- makeTcpClient loger "127.0.0.1" 5000 pDescribe
    notify output $ Outbox ("ping" :: ByteString)
    wait success
