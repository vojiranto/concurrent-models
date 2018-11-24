{-# LANGUAGE FlexibleContexts     #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Control.Concurrent.Actor
    ( Actor
    , ActorL
    , runActor
    , stopActor
    , killActor
    , Listener (..)
    , this
    , math
    , otherwiseMath
    , fromActorMessage
    , ToType(..)
    ) where

import           Universum
import           Data.This
import           Data.Describe
import           Control.Lens.At (at)
import           Control.Concurrent.Listener
import           Control.Concurrent.Loger
import           Control.Concurrent.Actor.ActorRuntime 
import           Control.Concurrent.Actor.Language
import           Control.Concurrent.Actor.Message
import           Control.Concurrent.STM.TChan
import           Control.Concurrent hiding (MVar, putMVar, takeMVar, newMVar)

data StopActor = StopActor

stopType :: MessageType
stopType = toType StopActor

data AnlyzeMessageResult = StopNodeR | ApplyHandlerR
    deriving Eq

analyzeMessage :: ActorMessage -> AnlyzeMessageResult
analyzeMessage message
    | toType message == stopType = StopNodeR
    | otherwise                  = ApplyHandlerR

applyHandler :: ActorRuntimeData -> ActorMessage -> IO ()
applyHandler actRuntime message = do
    let messageType = toType message
    case actRuntime ^. handlers .at messageType of
        Just handler -> handler message
        _            -> do
            let mHandler = actRuntime ^. handlers . at otherwiseType 
            whenJust mHandler $ \handler -> handler message
            unless (isJust mHandler) $ (actRuntime ^. loger) "[error] handler does not exist, msg is droped." 

-- | Build and run new actor.
runActor :: Loger -> ActorL () -> IO Actor
runActor logerAction handlerMap = do
    chan     <- atomically newTChan
    threadId <- initActor logerAction chan handlerMap 
    pure $ Actor chan threadId


initActor :: Loger -> TChan ActorMessage -> ActorL () -> IO ThreadId
initActor logerAction chan handlerMap = forkIO $ do
    actor      <- Actor chan <$> myThreadId
    actRuntime <- newActorRuntime logerAction actor handlerMap
    actorWorker actRuntime actor

actorWorker :: ActorRuntimeData -> Actor -> IO ()
actorWorker actRuntime actor@(Actor chan _) = do
    message <- atomically $ readTChan chan
    (actRuntime ^. loger) $ "[message] " <> describe message
    case analyzeMessage message of
        StopNodeR       -> killActor actor
        ApplyHandlerR   -> do
            applyHandler actRuntime message
            actorWorker  actRuntime actor



instance Typeable msg => Listener Actor msg where
    notify (Actor chan _) message = atomically $ writeTChan chan $ toActorMessage message

-- | Send stop msg to the actor.
stopActor :: Listener actor StopActor => actor -> IO ()
stopActor actor = notify actor StopActor

-- | Send async exeption to the actor.
killActor :: Actor -> IO ()
killActor (Actor _ threadId) = killThread threadId