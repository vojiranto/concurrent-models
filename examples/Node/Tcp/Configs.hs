module Node.Tcp.Configs where

import           Universum
import qualified Data.Configurator as Cfg
import qualified Data.Configurator.Types as Cfg
import           Control.Concurrent.Node.Network.Tcp

data ServerConfig = ServerConfig PortNumber Int

getServerConfig :: Cfg.Config -> IO ServerConfig
getServerConfig cfg = do
    let serverCfg = Cfg.subconfig "tcp_server" cfg
    portNumber      <- Cfg.require serverCfg "port_number"
    maxPackageSize  <- Cfg.require serverCfg "max_package_size"
    pure $ ServerConfig (toEnum portNumber) maxPackageSize


type Host         = String
data ClientConfig = ClientConfig Host ServerConfig

getClientConfig :: Cfg.Config -> IO ClientConfig
getClientConfig cfg = do
    let clientCfg = Cfg.subconfig "tcp_client" cfg
    serverCfg   <- getServerConfig clientCfg
    serverHost  <- Cfg.require clientCfg "server_host"
    pure $ ClientConfig serverHost serverCfg
