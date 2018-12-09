{-# Language FlexibleContexts #-}
{-# Language TemplateHaskell  #-}
{-# Language QuasiQuotes      #-}
module Control.Concurrent.Service.StreamController where

import           Control.Concurrent.Prelude
import           Control.Concurrent.Model
import           Control.Monad.Extra hiding (whenJust)
import qualified Data.ByteString as B
import qualified GHC.IO.Handle as H
import           Control.Concurrent.Service.Subscription
import qualified Network.Socket as S
import qualified Network        as S hiding (accept)
import           Control.Concurrent.Service.StreamController.Domain

newtype IsClosed = IsClosed TextId
data HandleClosed   = HandleClosed
data SocketClosed   = SocketClosed
data CommandClose   = CommandClose
data Message msg    = Message TextId msg 
newtype Inbox msg   = Inbox msg
newtype Outbox msg  = Outbox msg

packegeDescribe :: Int -> (msg -> ByteString) -> (ByteString -> Maybe msg) -> PackegeDescribe msg
packegeDescribe = PackegeDescribe

makeStates ["Opened", "Closed"]
makeFsm "StreamController" [[t|CommandClose|], [t|Subscription|], [t|Unsubscribe|]]

newtype NewConnect = NewConnect StreamController

instance Typeable msg => Listener StreamController (Outbox msg) where
    notify (StreamController fsm) = notify fsm

instance HaveTextId StreamController where
    getTextId (StreamController fsm) = getTextId fsm

streamController
    :: forall msg. Typeable msg
    => Loger -> Handle -> PackegeDescribe msg -> IO StreamController
streamController loger handler pDescribe = runFsm loger Opened $ do
    toLog Info "Start of stream controller"
    subscribers <- subscriptioService
    myRef <- this
    liftIO $ void $ forkIO $ whileM $ do
        rawData <- B.hGetSome handler (getMaxSize pDescribe)
        let retryReading = not $ B.null rawData
        whenJust (getDecoder pDescribe rawData) $ notify myRef . Inbox
        unless retryReading $ notify myRef HandleClosed
        pure retryReading

    math $ \((Inbox message) :: Inbox msg) ->
        multicast subscribers $ Message (getTextId myRef) message
    math $ \((Outbox msg) :: Outbox msg)   ->
        catchAny
            (B.hPut handler $ getEncoder pDescribe msg)
            (\_ -> notify myRef HandleClosed)

    closeLogic subscribers myRef $ H.hClose handler

closeLogic
    :: (HaveTextId a, Acception Closed (IO b))
    => IORef Subscribers -> a -> IO b -> StateMachineL ()
closeLogic subscribers myRef ioAction = do
    ifE HandleClosed $ Opened >-> Closed
    ifE CommandClose $ Opened >-> Closed
    addFinalState Closed
    onEntry Closed $ do
        multicast subscribers $ IsClosed $ getTextId myRef
        ioAction

makeTcpClient
    :: Typeable msg
    => Loger -> S.HostName -> S.PortNumber -> PackegeDescribe msg -> IO StreamController
makeTcpClient loger host port pDescribe =
    catchAny (do
        handle <- makeTcpConnection host port
        streamController loger handle pDescribe
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
    :: forall msg a.(
        HaveTextId a,
        Typeable msg,
        Listener a Subscription,
        Listener a NewConnect,
        Listener a (Message msg),
        Listener a IsClosed)
    => Loger -> a -> S.PortNumber -> PackegeDescribe msg -> IO TcpServer
makeTcpServer loger centralActor port pDescribe =
    catchAny (do
        listenSock <- S.listenOn (S.PortNumber port)
        tcpServer loger centralActor listenSock pDescribe
    ) (\exception -> do
        loger Error $ "Fail of tcp server start: " <> show exception
        runFsm loger Closed $ addFinalState Closed
    )

tcpServer
    :: forall msg a.(
    HaveTextId a,
    Typeable msg,
    Listener a Subscription,
    Listener a NewConnect,
    Listener a (Message msg),
    Listener a IsClosed)
    => Loger -> a -> S.Socket -> PackegeDescribe msg -> IO TcpServer
tcpServer loger centralActor listenSock pDescribe = runFsm loger Opened $ do
    toLog Info "Start of tcp server"
    subscribers <- subscriptioService
    fsmLoger    <- getLoger
    myRef       <- this
    liftIO $ void $ forkIO $
        whileM $ catchAny (do
            (clientSock, _) <- S.accept listenSock
            handler     <- S.socketToHandle clientSock ReadWriteMode
            sController <- streamController loger handler pDescribe
            -- sending events           from        to
            $(subscribe [t|IsClosed|])  sController centralActor
            $(subscribe [t|Message msg|]) sController centralActor
            notify centralActor $ NewConnect sController
            pure True
        ) (\exception -> do
            fsmLoger Error $ "Fail of connect accepting: " <> show exception
            notify myRef SocketClosed
            pure False
        )
    closeLogic subscribers myRef $ S.close listenSock