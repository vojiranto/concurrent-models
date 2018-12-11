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
    let maxPSize = 50
    controller <- runStateMachine loger ConnectManager $ do
        toLog Info "Start of bracast server"
        -- work logic
        connectsRef <- connectManager
        math $ \(Message _ msg) -> broadcast connectsRef msg
        
        -- finally logic
        ifE CommandClose $ ConnectManager >-> Closed
        addFinalState Closed
        onEntry Closed $ broadcast connectsRef CommandClose
    server <- makeTcpServer loger controller 5000 maxPSize
    pure (server, controller)

broadcast :: (Listener (Val t) msg, ToPairs t) => IORef t -> msg -> IO ()
broadcast connectsRef msg = do
    connects <- readIORef connectsRef
    forM_ (elems connects) $
        \connect -> notify connect msg