{-# Language FlexibleContexts #-}
{-# Language TemplateHaskell  #-}
{-# Language QuasiQuotes      #-}
{-# Language TypeFamilies     #-}

module Control.Concurrent.Service.ByteStream where

import           Control.Concurrent.Prelude
import           Control.Concurrent.Model
import           Control.Concurrent.Service.Subscription
    
newtype IsClosed  = IsClosed TextId
data CommandClose = CommandClose
data StreamManager = StreamManager 

makeStates ["Opened", "Closed"]
makeFsm "ByteStream" [[t|CommandClose|], [t|ByteString|], [t|Subscription|], [t|Unsubscribe|]]

instance HaveTextId ByteStream where
    getTextId (ByteStream fsm) = getTextId fsm

class (Listener s CommandClose, Typeable s, HaveTextId s) => Stream s where
    data family Message s
    data family Inbox s
    data family NewHandle s

    fromHandler :: NewHandle s -> s

instance Stream ByteStream where
    data Message   ByteStream = ByteStreamMessage TextId ByteString 
    data Inbox     ByteStream = ByteStreamInbox ByteString
    data NewHandle ByteStream = ByteStreamNewHandle ByteStream

    fromHandler (ByteStreamNewHandle byteStream) = byteStream

streemCloseLogic
    :: (HaveTextId a, Acception Closed (IO b))
    => IORef Subscribers -> a -> IO b -> StateMachineL ()
streemCloseLogic subscribers myRef ioAction = do
    ifE CommandClose $ Opened >-> Closed
    addFinalState Closed
    onEntry Closed $ do
        multicast subscribers $ IsClosed $ getTextId myRef
        ioAction

streemSubscribe
    :: (Listener a (NewHandle ByteStream)
    , Listener a (Message ByteStream)
    , Listener a IsClosed
    , HaveTextId a)
    => ByteStream -> a -> IO ()
streemSubscribe stream centralActor = do
    -- sending events          from   to
    $(subscribe [t|IsClosed|]) stream centralActor
    $(subscribe [t|Message ByteStream|])  stream centralActor
    notify centralActor $ ByteStreamNewHandle stream