module Main where

import           Universum
import           System.Environment
import qualified Data.Configurator as Cfg
import           Data.Text (pack)
import           Control.Concurrent.Node.Loger
import           Node.Tcp.Configs
import           Node

main :: IO ()
main = do
    logActor <- runLogerActor
    void $ сonsoleLogOn logActor
    args <- getArgs
    case args of
        "tcp_client":_           -> catchAny (do
                cfg       <- Cfg.load ["configs/tcp_client.cfg"]
                clientCfg <- getClientConfig cfg
                tcpClient (loger logActor) clientCfg
            ) (putTextLn . show)
        "tcp_broadcast_server":_ -> catchAny (do
                cfg       <- Cfg.load ["configs/tcp_server.cfg"]
                serverCfg <- getServerConfig cfg 
                tcpBroadcastServer (loger logActor) serverCfg
            ) (putTextLn . show)
        key:_ -> putTextLn $
            "The key \"" <> pack key <> "\" isn't supported."
