{-# Language TemplateHaskell  #-}
{-# Language QuasiQuotes      #-}
{-# Language FlexibleContexts #-}
module Node.Tcp.BroadcastServer where

import           Universum

import           Control.Concurrent.Model
import           Control.Concurrent.Node.Network.Tcp
import           Control.Concurrent.Node.Console
import           Control.Concurrent.Flag
import           Control.Concurrent.Service.Subscription
import           Node.Tcp.Configs

tcpBroadcastServer :: Loger -> ServerConfig -> IO ()
tcpBroadcastServer loger serverConfig  = do
    (tcpServer, controller) <- runBroadcastServer loger serverConfig
    console                 <- runConsoleWorker loger
    exitFromServer <- newFlag
    commandReader <- runActor loger $
        math $ \(Message _ msg) -> when (msg == "exit") $ do
            notify tcpServer  CommandClose
            notify controller CommandClose
            liftFlag exitFromServer
    $(subscribe [t|Message|]) console commandReader
    wait exitFromServer

runBroadcastServer :: Loger -> ServerConfig -> IO (TcpServer, StateMachine)
runBroadcastServer loger (ServerConfig portNumber maxPSize) = do
    controller <- runStateMachine loger StreamManager $ do
        toLog Info "Start of broadcast server"
        connectsRef <- streamManager
        math $ \(Message _ msg) -> broadcast connectsRef msg

    server <- runTcpServer loger controller portNumber maxPSize
    pure (server, controller)
