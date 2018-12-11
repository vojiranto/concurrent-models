{-# Language TemplateHaskell  #-}
{-# Language QuasiQuotes      #-}
{-# Language FlexibleContexts #-}
module Nodes.Tcp.BroadcastServer where

import           Universum

import           Control.Concurrent.Model
import           Control.Concurrent.Node.Loger
import           Control.Concurrent.Node.Network.Tcp
import           Control.Concurrent.Node.Console
import           Control.Concurrent.Flag
import           Control.Concurrent.Service.Subscription

broadcastServer :: IO ()
broadcastServer = do
    (tcpServer, controller) <- makeBroadcastServer
    console                 <- runConsoleWorker loger
    exitFromServer <- newFlag
    commandReader <- runActor loger $
        math $ \(Message _ msg) -> when (msg == "exit") $ do
            notify tcpServer  CommandClose
            notify controller CommandClose
            liftFlag exitFromServer
    $(subscribe [t|Message|]) console commandReader
    wait exitFromServer

makeBroadcastServer :: IO (TcpServer, StateMachine)
makeBroadcastServer = do
    let maxPSize   = 500
    let portNumber = 5000
    controller <- runStateMachine loger StreamManager $ do
        toLog Info "Start of broadcast server"
        connectsRef <- streamManager
        math $ \(Message _ msg) -> broadcast connectsRef msg

    server <- runTcpServer loger controller portNumber maxPSize
    pure (server, controller)
