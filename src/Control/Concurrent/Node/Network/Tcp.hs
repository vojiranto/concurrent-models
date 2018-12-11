{-# Language FlexibleContexts #-}
{-# Language TemplateHaskell  #-}
{-# Language QuasiQuotes      #-}
module Control.Concurrent.Node.Network.Tcp
    ( module X
    , makeTcpServer
    , makeTcpClient
    ) where

import           Control.Concurrent.Prelude
import           Control.Concurrent.Model
import           Control.Monad.Extra hiding (whenJust)
import qualified Data.ByteString as B
import qualified GHC.IO.Handle as H
import           Control.Concurrent.Service.Subscription
import qualified Network.Socket as S
import qualified Network        as S hiding (accept)
import           Control.Concurrent.Service.Network as X

makeStream :: Loger -> Handle -> Int -> IO Stream
makeStream loger handler maxPSize = runFsm loger Opened $ do
    toLog Info "Start of stream controller"
    subscribers <- subscriptioService
    myRef       <- this
    liftIO $ readerWorker (B.hGetSome handler maxPSize) myRef
    math $ \(Inbox message) ->
        multicast subscribers $ Message (getTextId myRef) message
    math $ \msg -> catchAny
        (B.hPut handler msg)
        (\_ -> notify myRef CommandClose)

    closeLogic subscribers myRef $ H.hClose handler


makeTcpClient
    :: Loger -> S.HostName -> S.PortNumber -> Int -> IO Stream
makeTcpClient loger host port maxPSize =
    catchAny (do
        handle <- makeTcpConnection host port
        makeStream loger handle maxPSize
    ) (\exception -> do
        loger Warn $ "Fail of start tcp client: " <> show exception
        runFsm loger Closed $ addFinalState Closed
    )

makeTcpConnection :: S.HostName -> S.PortNumber -> IO Handle
makeTcpConnection host port = do
    address    <- head <$> S.getAddrInfo Nothing (Just host) (Just $ show port)
    sock       <- S.socket (S.addrFamily address) S.Stream S.defaultProtocol
    S.connect sock $ S.addrAddress address
    S.socketToHandle sock ReadWriteMode

makeFsm "TcpServer" [[t|CommandClose|], [t|Subscription|], [t|Unsubscribe|]]

makeTcpServer
    :: forall a.(
        HaveTextId a,
        Listener a NewConnect,
        Listener a Message,
        Listener a IsClosed)
    => Loger -> a -> S.PortNumber -> Int -> IO TcpServer
makeTcpServer loger centralActor port maxPSize =
    catchAny (do
        listenSock <- S.listenOn (S.PortNumber port)
        tcpServer loger centralActor listenSock maxPSize
    ) (\exception -> do
        loger Error $ "Fail of tcp server start: " <> show exception
        runFsm loger Closed $ addFinalState Closed
    )

tcpServer
    :: forall a.(
    HaveTextId a,
    Listener a NewConnect,
    Listener a Message,
    Listener a IsClosed)
    => Loger -> a -> S.Socket -> Int -> IO TcpServer
tcpServer loger centralActor listenSock maxPSize = runFsm loger Opened $ do
    toLog Info "Start of tcp server"
    subscribers <- subscriptioService
    fsmLoger    <- getLoger
    myRef       <- this
    liftIO $ void $ forkIO $
        whileM $ catchAny (do
            (clientSock, _) <- S.accept listenSock
            handler     <- S.socketToHandle clientSock ReadWriteMode
            sController <- makeStream loger handler maxPSize
            -- sending events          from        to
            $(subscribe [t|IsClosed|]) sController centralActor
            $(subscribe [t|Message|])  sController centralActor
            notify centralActor $ NewConnect sController
            pure True
        ) (\exception -> do
            fsmLoger Error $ "Fail of connect accepting: " <> show exception
            notify myRef CommandClose
            pure False
        )
    closeLogic subscribers myRef $ S.close listenSock