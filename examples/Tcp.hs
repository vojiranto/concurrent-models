{-# Language TemplateHaskell  #-}
{-# Language QuasiQuotes      #-}
module Tcp where

import           Universum

import           Control.Concurrent.Model
import           Control.Concurrent.Node.Network.Tcp
import           Control.Concurrent.Flag

tcpExample :: Loger -> IO ()
tcpExample loger = do
    success <- newFlag
    let maxPSize = 50
    controller <- runStateMachine loger StreamManager $ do
        toLog Info "Start of central controller"
        void streamManager
        math $ \(Message _ msg) ->
            when (msg == "ping") $ liftFlag success
    void $ runTcpServer loger controller 5000 maxPSize
    output <- runTcpClient loger "127.0.0.1" 5000 maxPSize
    notify output ("ping" :: ByteString)
    wait success
