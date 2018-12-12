{-# Language TemplateHaskell  #-}
{-# Language QuasiQuotes      #-}
{-# Language FlexibleContexts #-}
module Node.Tcp.Client where

import           Universum

import           Control.Concurrent.Model
import           Control.Concurrent.Flag
import           Control.Concurrent.Service.Subscription
import           Control.Concurrent.Node.Loger
import           Control.Concurrent.Node.Network.Tcp
import           Control.Concurrent.Node.Console

tcpClient :: IO ()
tcpClient = do
    let maxPSize   = 500
    exitFromClient  <- newFlag
    client          <- runTcpClient loger "127.0.0.1" 5000 maxPSize
    console         <- runConsoleWorker loger

    resenderToConsole <- runActor loger $
        math $ \(Message _ msg) -> notify console msg

    resenderToClient <- runActor loger $ do
        math $ \(Message _ msg) -> do
            when (msg == "exit") $ liftFlag exitFromClient
            notify client msg
        math $ \(IsClosed _) -> liftFlag exitFromClient

    $(subscribe [t|Message|])  console   resenderToClient 
    $(subscribe [t|Message|])  client    resenderToConsole
    $(subscribe [t|IsClosed|]) client    resenderToClient

    clientState <- readState client
    when (clientState `is` Closed) $ liftFlag exitFromClient
    wait exitFromClient