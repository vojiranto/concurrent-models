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

newtype IsClosed = IsClosed TextId 
data HandleClosed   = HandleClosed
data SocketClosed   = SocketClosed
data CommandClose   = CommandClose
data Message msg    = Message TextId msg 
newtype Inbox msg   = Inbox msg
newtype Outbox msg  = Outbox msg

makeStates ["Opened", "Closed"]
makeFsm "StreamController" [[t|CommandClose|], [t|Subscription|], [t|Unsubscribe|]]

instance Typeable msg => Listener StreamController (Outbox msg) where
    notify (StreamController fsm) = notify fsm

instance HaveTextId StreamController where
    getTextId (StreamController fsm) = getTextId fsm


data PackegeDescribe msg = PackegeDescribe Int (msg -> ByteString) (ByteString -> Maybe msg)

getEncoder :: PackegeDescribe msg -> msg -> ByteString
getEncoder (PackegeDescribe _ encoder _) = encoder

getDecoder :: PackegeDescribe msg -> ByteString -> Maybe msg
getDecoder (PackegeDescribe _ _ decoder) = decoder

getMaxSize :: PackegeDescribe msg -> Int
getMaxSize (PackegeDescribe maxSize _ _) = maxSize


streamController
    :: forall msg. Typeable msg
    => Loger -> Handle -> PackegeDescribe msg -> IO StreamController
streamController loger handler pDescribe = runFsm loger Opened $ do
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

makeTcpConnection :: S.HostName -> S.PortNumber -> IO Handle
makeTcpConnection host port = do
    address    <- head <$> S.getAddrInfo Nothing (Just host) (Just $ show port)
    sock       <- S.socket (S.addrFamily address) S.Stream S.defaultProtocol
    catchAny (S.connect sock $ S.addrAddress address) (\_ -> pure ())
    S.socketToHandle sock ReadWriteMode

makeFsm "TcpServer" [[t|CommandClose|], [t|Subscription|], [t|Unsubscribe|]]

newtype NewConnect = NewConnect StreamController

makeTcpServer
    :: forall msg a.(
        HaveTextId a,
        Typeable msg,
        Listener a Subscription,
        Listener a NewConnect,
        Listener a (Inbox msg),
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
    Listener a (Inbox msg),
    Listener a IsClosed)
    => Loger -> a -> S.Socket -> PackegeDescribe msg -> IO TcpServer
tcpServer loger centralActor listenSock pDescribe = runFsm loger Opened $ do
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
            $(subscribe [t|Inbox msg|]) sController centralActor
            notify centralActor $ NewConnect sController
            pure True
        ) (\exception -> do
            fsmLoger Error $ "Fail of connect accepting: " <> show exception
            notify myRef SocketClosed
            pure False
        )
    closeLogic subscribers myRef $ S.close listenSock