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

tcpBroadcastServer :: Loger -> IO ()
tcpBroadcastServer loger = do
    (tcpServer, controller) <- runBroadcastServer loger 5000 500
    console                 <- runConsoleWorker loger
    exitFromServer <- newFlag
    commandReader <- runActor loger $
        math $ \(Message _ msg) -> when (msg == "exit") $ do
            notify tcpServer  CommandClose
            notify controller CommandClose
            liftFlag exitFromServer
    $(subscribe [t|Message|]) console commandReader
    wait exitFromServer

runBroadcastServer :: Loger -> PortNumber -> Int -> IO (TcpServer, StateMachine)
runBroadcastServer loger portNumber maxPSize = do
    controller <- runStateMachine loger StreamManager $ do
        toLog Info "Start of broadcast server"
        connectsRef <- streamManager
        math $ \(Message _ msg) -> broadcast connectsRef msg

    server <- runTcpServer loger controller portNumber maxPSize
    pure (server, controller)
