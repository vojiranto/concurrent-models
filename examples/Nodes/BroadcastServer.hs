{-# Language TemplateHaskell  #-}
{-# Language QuasiQuotes      #-}
{-# Language FlexibleContexts #-}
module Nodes.BroadcastServer where

import           Universum

import           Control.Concurrent.Model
import           Control.Concurrent.Node.Loger
import           Control.Concurrent.Node.Network.Tcp

broadcastServer :: IO (TcpServer, StateMachine)
broadcastServer = do
    let maxPSize   = 500
    let portNumber = 5000
    controller <- runStateMachine loger StreamManager $ do
        toLog Info "Start of broadcast server"
        connectsRef <- streamManager
        math $ \(Message _ msg) -> broadcast connectsRef msg

    server <- makeTcpServer loger controller portNumber maxPSize
    pure (server, controller)
