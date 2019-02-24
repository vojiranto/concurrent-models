{-# Language TemplateHaskell  #-}
{-# Language QuasiQuotes      #-}
{-# Language FlexibleContexts #-}
module Tests.BroadcastServer where

import           Universum
import           Control.Concurrent.Node.Network.Tcp
import           Control.Concurrent.Model
import           Node.Tcp.BroadcastServer
import           Node.Tcp.Configs
import           Control.Concurrent.Flag
import           Control.Concurrent.Service.Subscription


broadcastServerTest :: Loger -> PortNumber -> Int -> IO ()
broadcastServerTest loger portNumber maxPSize = do
    (server, controller) <- runBroadcastServer loger (ServerConfig portNumber maxPSize)
    client               <- runTcpClient loger "127.0.0.1" portNumber maxPSize
    exitFromServer <- newFlag
    commandReader <- runActor loger $
        math $ \(ByteStreamMessage _ msg) -> when (msg == "ping") $ do
            notify server     CommandClose
            notify controller CommandClose
            liftFlag exitFromServer
    $(subscribe [t|Message ByteStream|]) client commandReader
    notify client ("ping" :: ByteString) 
    wait exitFromServer