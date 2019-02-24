{-# Language FlexibleContexts #-}
{-# Language TemplateHaskell  #-}
{-# Language QuasiQuotes      #-}

module Control.Concurrent.Service.StreamManager where

import           Control.Concurrent.Prelude
import           Control.Concurrent.Model
import           Control.Concurrent.Service.ByteStream

import qualified Data.Map as M

streamManager :: StateMachineL (IORef (M.Map TextId ByteStream))
streamManager = do
    toLog Info "Init connect manager service"
    -- work logic
    connectsRef <- liftIO $ newIORef mempty 
    mathS StreamManager $ \(NewHandle fsm) ->
        void $ modifyIORef' connectsRef (M.insert (getTextId fsm) fsm)
    mathS StreamManager $ \(IsClosed textId) ->
        void $ modifyIORef' connectsRef (M.delete textId)

    -- finally logic
    addFinalState Closed
    ifE CommandClose $ StreamManager >-> Closed
    onExit StreamManager $ do
        broadcast connectsRef CommandClose
        writeIORef connectsRef mempty

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