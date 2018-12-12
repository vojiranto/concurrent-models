module Main where

import           Universum
import           Control.Concurrent.Node.Loger
import           Node

main :: IO ()
main = do
    void сonsoleLogOn
    args <- getArgs
    case args of
        "tcp_client":_           -> tcpClient
        "tcp_broadcast_server":_ -> tcpBroadcastServer
        _                        -> pure ()
