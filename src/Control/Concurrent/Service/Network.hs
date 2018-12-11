{-# Language FlexibleContexts #-}
{-# Language TemplateHaskell  #-}
{-# Language QuasiQuotes      #-}

module Control.Concurrent.Service.Network
    (module X
    , Message(..)
    , Inbox(..)
    , readerWorker
    , subscribeStreem
    , closeLogic
    ) where

import           Control.Concurrent.Prelude
import           Control.Concurrent.Model
import           Control.Monad.Extra hiding (whenJust)
import qualified Data.ByteString as B
import           Control.Concurrent.Service.Subscription
import           Control.Concurrent.Service.Network.ConnectManager as X

data Message      = Message TextId ByteString 
newtype Inbox     = Inbox ByteString

readerWorker
    :: (Listener a CommandClose, Listener a Inbox, Listener a ByteString)
    => IO ByteString -> a -> IO ()
readerWorker readerAction myRef = void $ forkIO $ whileM $ do
    rawData <- readerAction
    let retryReading = not $ B.null rawData
    if retryReading
        then notify myRef (Inbox rawData)
        else notify myRef CommandClose
    pure retryReading

closeLogic
    :: (HaveTextId a, Acception Closed (IO b))
    => IORef Subscribers -> a -> IO b -> StateMachineL ()
closeLogic subscribers myRef ioAction = do
    ifE CommandClose $ Opened >-> Closed
    addFinalState Closed
    onEntry Closed $ do
        multicast subscribers $ IsClosed $ getTextId myRef
        ioAction

subscribeStreem
    :: (Listener a NewConnect
    , Listener a Message
    , Listener a IsClosed
    , HaveTextId a)
    => Stream -> a -> IO ()
subscribeStreem stream centralActor = do
    -- sending events          from   to
    $(subscribe [t|IsClosed|]) stream centralActor
    $(subscribe [t|Message|])  stream centralActor
    notify centralActor $ NewConnect stream