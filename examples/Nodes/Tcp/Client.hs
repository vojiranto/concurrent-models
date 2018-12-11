{-# Language TemplateHaskell  #-}
{-# Language QuasiQuotes      #-}
{-# Language FlexibleContexts #-}
module Nodes.Tcp.Client where

import           Universum

import           Control.Concurrent.Model
import           Control.Concurrent.Flag
import           Control.Concurrent.Service.Subscription
import           Control.Concurrent.Node.Loger
import           Control.Concurrent.Node.Network.Tcp
import           Control.Concurrent.Node.Console

client :: IO ()
client = do
    let maxPSize   = 500
    exitFromClient  <- newFlag
    tcpClient       <- runTcpClient loger "127.0.0.1" 5000 maxPSize
    console         <- runConsoleWorker loger

    resenderToConsole <- runActor loger $
        math $ \(Message _ msg) -> notify console msg

    resenderToClient <- runActor loger $
        math $ \(Message _ msg) -> do
            when (msg == "XXX") $ liftFlag exitFromClient
            notify tcpClient msg

    $(subscribe [t|Message|])  console   resenderToClient 
    $(subscribe [t|Message|])  tcpClient resenderToConsole
    wait exitFromClient