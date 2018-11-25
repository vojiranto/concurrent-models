{-# LANGUAGE FlexibleContexts     #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Control.Concurrent.Actor
    ( Actor
    , ActorL
    , Role(..)
    , runActor
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

class Role actor where
    -- | Build and run new actor.
    runRole  :: Loger -> ActorL () -> IO actor
    -- | Send stop msg to the actor.
    stopRole :: actor -> IO ()
    -- | Send async exeption to the actor.
    killRole :: actor -> IO ()

-- | Build and run new actor.
runActor :: Loger -> ActorL () -> IO Actor
runActor = runRole

instance Role Actor where
    runRole logerAction handlerMap = do
        chan     <- atomically newTChan
        threadId <- initActor logerAction chan handlerMap 
        pure $ Actor chan threadId

    stopRole actor = notify actor StopActor
    killRole (Actor _ threadId) = killThread threadId

initActor :: Loger -> TChan ActorMessage -> ActorL () -> IO ThreadId
initActor logerAction chan handlerMap = forkIO $ do
    actor      <- Actor chan <$> myThreadId
    actRuntime <- newActorRuntime logerAction actor handlerMap
    actorWorker actRuntime actor

actorWorker :: ActorRuntimeData -> Actor -> IO ()
actorWorker actRuntime actor = do
    message <- recieveMessage actor
    (actRuntime ^. loger) $ "[message] " <> describe message
    case analyzeMessage message of
        StopNodeR       -> killRole actor
        ApplyHandlerR   -> do
            applyHandler actRuntime message
            actorWorker  actRuntime actor

data AnlyzeMessageResult = StopNodeR | ApplyHandlerR
    deriving Eq

analyzeMessage :: ActorMessage -> AnlyzeMessageResult
analyzeMessage message
    | toType message == stopType = StopNodeR
    | otherwise                  = ApplyHandlerR

applyHandler :: ActorRuntimeData -> ActorMessage -> IO ()
applyHandler actRuntime message = do
    let messageType = toType message
    case actRuntime ^. handlers . at messageType of
        Just handler -> handler message
        _            -> do
            let mHandler = actRuntime ^. handlers . at otherwiseType 
            whenJust mHandler $ \handler -> handler message
            unless (isJust mHandler) $ (actRuntime ^. loger) "[error] handler does not exist, msg is droped." 

instance Typeable msg => Listener Actor msg where
    notify (Actor chan _) message = atomically $ writeTChan chan $ toActorMessage message

data StopActor = StopActor

stopType :: MessageType
stopType = toType StopActor