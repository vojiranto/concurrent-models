{-# Language FlexibleContexts #-}
{-# Language TemplateHaskell  #-}
{-# Language QuasiQuotes      #-}

module Control.Concurrent.Service.Stream where

import           Control.Concurrent.Prelude
import           Control.Concurrent.Model
import           Control.Concurrent.Service.Subscription
    
newtype IsClosed  = IsClosed TextId
data CommandClose = CommandClose
data StreamManager = StreamManager 

makeStates ["Opened", "Closed"]
makeFsm "Stream" [[t|CommandClose|], [t|ByteString|], [t|Subscription|], [t|Unsubscribe|]]

instance HaveTextId Stream where
    getTextId (Stream fsm) = getTextId fsm

data Message      = Message TextId ByteString 
newtype Inbox     = Inbox ByteString

newtype NewHandle = NewHandle Stream

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
    :: (Listener a NewHandle
    , Listener a Message
    , Listener a IsClosed
    , HaveTextId a)
    => Stream -> a -> IO ()
subscribeStreem stream centralActor = do
    -- sending events          from   to
    $(subscribe [t|IsClosed|]) stream centralActor
    $(subscribe [t|Message|])  stream centralActor
    notify centralActor $ NewHandle stream