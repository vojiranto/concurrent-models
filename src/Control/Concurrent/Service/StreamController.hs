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

streamController
    :: forall msg. Typeable msg
    => Loger -> Handle -> Int -> (msg -> ByteString) -> (ByteString -> Maybe msg) -> IO StreamController
streamController loger handler maxPackSize encoder decoder =
    runFsm loger Opened $ do
        subscribers <- subscriptioService
        myRef <- this
        liftIO $ void $ forkIO $ whileM $ do
            rawData <- B.hGetSome handler maxPackSize
            let retryReading = not $ B.null rawData
            whenJust (decoder rawData) $ notify myRef . Inbox
            unless retryReading $ notify myRef HandleClosed
            pure retryReading

        math $ \((Inbox message) :: Inbox msg) ->
            multicast subscribers $ Message (getTextId myRef) message
        math $ \((Outbox msg) :: Outbox msg)   ->
            catchAny
                (B.hPut handler $ encoder msg)
                (\_ -> notify myRef HandleClosed)

        ifE HandleClosed $ Opened >-> Closed
        ifE CommandClose $ Opened >-> Closed
        addFinalState Closed
        onEntry Closed $ do
            multicast subscribers $ IsClosed $ getTextId myRef
            H.hClose handler

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
    => Loger -> a -> S.PortNumber -> Int -> (msg -> ByteString) -> (ByteString -> Maybe msg) -> IO TcpServer

makeTcpServer loger centralActor port maxPackSize encoder decoder = do
    eSock <- try $ S.listenOn (S.PortNumber port)
    case eSock of
        Left (exception :: SomeException) -> do
            loger Error $ "Fail of tcp server start: " <> show exception
            runFsm loger Closed $ addFinalState Closed
        Right listenSock -> runFsm loger Opened $ do
            subscribers <- subscriptioService
            fsmLoger    <- getLoger
            myRef       <- this
            liftIO $ void $ forkIO $ whileM $ do
                eClientSock <- try $ S.accept listenSock
                case eClientSock of
                    Left (exception :: SomeException) -> do
                        fsmLoger Error $ "Fail of connect accepting: " <> show exception
                        notify myRef SocketClosed
                    Right (clientSock, _) -> do
                        handler     <- S.socketToHandle clientSock ReadWriteMode
                        sController <- streamController loger handler maxPackSize encoder decoder
                        notify  sController $
                            makeNotify centralActor (\(isClosed :: IsClosed) -> notify centralActor isClosed)
                        notify  sController $
                            makeNotify centralActor (\(msg :: Inbox msg) -> notify centralActor msg)
                        notify centralActor $ NewConnect sController
                pure $ isRight eClientSock

            ifE SocketClosed $ Opened >-> Closed
            ifE CommandClose $ Opened >-> Closed
            addFinalState Closed
            onEntry Closed $ do
                multicast subscribers $ IsClosed $ getTextId myRef
                S.close listenSock
