module Main where

import           Universum
import           System.Environment
import qualified Data.Configurator as Cfg
import           Data.Text (pack)
import           Control.Concurrent.Node.Loger
import           Node

main :: IO ()
main = do
    logActor <- runLogerActor
    void $ сonsoleLogOn logActor
    args <- getArgs
    case args of
        "tcp_client":_           -> catchAny (do
                cfg             <- Cfg.load ["configs/tcp_client.cfg"]
                host            <- Cfg.require cfg "server_host"
                portNumber      <- Cfg.require cfg "port_number"
                maxPackageSize  <- Cfg.require cfg "max_package_size"
                tcpClient host (toEnum portNumber) maxPackageSize (loger logActor)
            ) (putTextLn .show)
        "tcp_broadcast_server":_ -> catchAny (do
                cfg             <- Cfg.load ["configs/tcp_server.cfg"]
                portNumber      <- Cfg.require cfg "port_number"
                maxPackageSize  <- Cfg.require cfg "max_package_size"
                tcpBroadcastServer (toEnum portNumber) maxPackageSize (loger logActor)
            ) (putTextLn .show)
        key:_ -> putTextLn $
            "The key \"" <> pack key <> "\" isn't supported."
