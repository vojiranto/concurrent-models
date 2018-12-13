module Main where

import           Universum
import           Control.Concurrent.Node.Loger
import           Node

main :: IO ()
main = do
    logActor <- runLogerActor
    void $ сonsoleLogOn logActor
    args <- getArgs
    case args of
        "tcp_client":_           -> tcpClient (loger logActor)
        "tcp_broadcast_server":_ -> tcpBroadcastServer (loger logActor)
        _                        -> pure ()
