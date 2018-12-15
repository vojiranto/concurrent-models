{-# Language TemplateHaskell  #-}
{-# Language QuasiQuotes      #-}
module Tcp where

import           Universum

import           Control.Concurrent.Model
import           Control.Concurrent.Node.Network.Tcp
import           Control.Concurrent.Flag

tcpExample :: Loger -> PortNumber -> Int -> IO ()
tcpExample loger portNumber maxPSize = do
    success <- newFlag
    controller <- runStateMachine loger StreamManager $ do
        toLog Info "Start of central controller"
        void streamManager
        math $ \(Message _ msg) ->
            when (msg == "ping") $ liftFlag success
    void $ runTcpServer loger controller portNumber maxPSize
    output <- runTcpClient loger "127.0.0.1" portNumber maxPSize
    notify output ("ping" :: ByteString)
    wait success
