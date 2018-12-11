{-# Language FlexibleContexts #-}
{-# Language TemplateHaskell  #-}
{-# Language QuasiQuotes      #-}

module Control.Concurrent.Service.Network.ConnectManager where

import           Control.Concurrent.Prelude
import           Control.Concurrent.Model
import qualified Data.Map as M
import           Control.Concurrent.Service.Subscription

newtype IsClosed  = IsClosed TextId
data CommandClose = CommandClose
data ConnectManager = ConnectManager 

makeStates ["Opened", "Closed"]
makeFsm "Stream" [[t|CommandClose|], [t|ByteString|], [t|Subscription|], [t|Unsubscribe|]]

newtype NewConnect = NewConnect Stream

instance HaveTextId Stream where
    getTextId (Stream fsm) = getTextId fsm

connectManager :: StateMachineL (IORef (M.Map TextId Stream))
connectManager = do
    toLog Info "Init connect manager service"
    -- work logic
    connectsRef <- liftIO $ newIORef mempty 
    mathS ConnectManager $ \(NewConnect fsm) ->
        void $ modifyIORef' connectsRef (M.insert (getTextId fsm) fsm)
    mathS ConnectManager $ \(IsClosed textId) ->
        void $ modifyIORef' connectsRef (M.delete textId)

    -- finally logic
    addFinalState Closed
    ifE CommandClose $ ConnectManager >-> Closed
    onEntry Closed $ broadcast connectsRef CommandClose

    pure connectsRef

broadcast :: (Listener (Val t) msg, ToPairs t) => IORef t -> msg -> IO ()
broadcast connectsRef msg = do
    connects <- readIORef connectsRef
    forM_ (elems connects) $
        \connect -> notify connect msg

sendTo :: (Listener a msg, Ord k) => IORef (Map k a) -> k -> msg -> IO ()
sendTo connectsRef textId msg = do
    connects <- readIORef connectsRef
    whenJust (M.lookup textId connects) $ \conn -> notify conn msg