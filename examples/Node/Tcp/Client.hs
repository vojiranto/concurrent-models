{-# Language TemplateHaskell  #-}
{-# Language QuasiQuotes      #-}
{-# Language FlexibleContexts #-}
module Node.Tcp.Client where

import           Universum

import           Control.Concurrent.Model
import           Control.Concurrent.Flag
import           Control.Concurrent.Service.Subscription
import           Control.Concurrent.Node.Network.Tcp
import           Control.Concurrent.Node.Console
import           Node.Tcp.Configs

tcpClient :: Loger -> ClientConfig -> IO ()
tcpClient loger (ClientConfig host (ServerConfig portNumber maxPSize)) = do
    exitFromClient  <- newFlag
    client          <- runTcpClient loger host portNumber maxPSize
    console         <- runConsoleWorker loger

    resenderToConsole <- runActor loger $
        math $ \(ByteStreamMessage _ msg) -> notify console msg

    resenderToClient <- runActor loger $ do
        math $ \(ByteStreamMessage _ msg) -> do
            when (msg == "exit") $ liftFlag exitFromClient
            notify client msg
        math $ \(IsClosed _) -> liftFlag exitFromClient

    $(subscribe [t|Message ByteStream|])  console   resenderToClient 
    $(subscribe [t|Message ByteStream|])  client    resenderToConsole
    $(subscribe [t|IsClosed|]) client    resenderToClient

    clientState <- readState client
    when (clientState `is` Closed) $ liftFlag exitFromClient
    wait exitFromClient