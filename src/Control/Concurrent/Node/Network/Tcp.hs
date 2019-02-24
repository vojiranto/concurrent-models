{-# Language FlexibleContexts #-}
{-# Language TemplateHaskell  #-}
{-# Language QuasiQuotes      #-}
module Control.Concurrent.Node.Network.Tcp
    ( module X
    , TcpServer(..)
    , runTcpServer
    , runTcpClient
    , S.PortNumber
    ) where

import           Control.Concurrent.Prelude
import           Control.Concurrent.Model
import           Control.Monad.Extra hiding (whenJust)
import qualified Data.ByteString as B
import qualified GHC.IO.Handle as H
import           Control.Concurrent.Service.Subscription
import qualified Network.Socket as S
import           Control.Concurrent.Service.StreamManager as X
import           Control.Concurrent.Service.ByteStream        as X

makeFsm "TcpServer" [[t|CommandClose|], [t|Subscription|], [t|Unsubscribe|]]
runTcpServer
    :: forall a.(
        HaveTextId a,
        Listener a (NewHandle ByteStream),
        Listener a (Message ByteStream),
        Listener a IsClosed)
    => Loger -> a -> S.PortNumber -> Int -> IO TcpServer
runTcpServer loger centralActor port maxPSize =
    catchAny (do
        address    <- head <$> S.getAddrInfo
            (Just (S.defaultHints {S.addrFlags = [S.AI_PASSIVE]}))
            Nothing
            (Just $ show port)
        listenSock <- S.socket (S.addrFamily address) S.Stream S.defaultProtocol
        S.bind listenSock (S.addrAddress address)
        S.listen listenSock 1
        tcpServer loger centralActor listenSock maxPSize
    ) (\exception -> do
        loger Error $ "Fail of tcp server start: " <> show exception
        runFsm loger Closed $ addFinalState Closed
    )

tcpServer
    :: forall a.(
    HaveTextId a,
    Listener a (NewHandle ByteStream),
    Listener a (Message ByteStream),
    Listener a IsClosed)
    => Loger -> a -> S.Socket -> Int -> IO TcpServer
tcpServer loger centralActor listenSock maxPSize = runFsm loger Opened $ do
    toLog Info "Start of tcp server"
    subscribers <- subscriptionService
    fsmLoger    <- getLoger
    myRef       <- this
    liftIO $ void $ forkIO $
        whileM $ catchAny (do
            (clientSock, _) <- S.accept listenSock
            handler         <- S.socketToHandle clientSock ReadWriteMode
            stream          <- runStream loger handler maxPSize
            streemSubscribe stream centralActor
            pure True
        ) (\_ -> do
            fsmLoger Info "Accepting socket is closed."
            notify myRef CommandClose
            pure False
        )
    streemCloseLogic subscribers myRef $ S.close listenSock

runTcpClient
    :: Loger -> S.HostName -> S.PortNumber -> Int -> IO ByteStream
runTcpClient loger host port maxPSize =
    catchAny (do
        handle <- makeTcpConnection host port
        runStream loger handle maxPSize
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

runStream :: Loger -> Handle -> Int -> IO ByteStream
runStream loger handler maxPSize = runFsm loger Opened $ do
    toLog Info "Start of stream controller"
    subscribers <- subscriptionService
    myRef       <- this
    liftIO $ readerWorker (B.hGetSome handler maxPSize) myRef
    math $ \(ByteStreamInbox message) ->
        multicast subscribers $ ByteStreamMessage (getTextId myRef) message
    math $ \msg -> catchAny
        (B.hPut handler msg)
        (\_ -> notify myRef CommandClose)

    streemCloseLogic subscribers myRef $ H.hClose handler

readerWorker
    :: (Listener a CommandClose, Listener a (Inbox ByteStream), Listener a ByteString)
    => IO ByteString -> a -> IO ()
readerWorker readerAction myRef = void $ forkIO $ whileM $ do
    rawData <- readerAction
    let retryReading = not $ B.null rawData
    if retryReading
        then notify myRef (ByteStreamInbox rawData)
        else notify myRef CommandClose
    pure retryReading
