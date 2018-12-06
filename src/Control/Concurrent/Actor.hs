{-# LANGUAGE FlexibleContexts     #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Control.Concurrent.Actor
    ( Actor
    , ActorL
    , Role(..)
    , Math(..)
    , runActor
    , otherwiseMath
    , makeAct
    ) where

import           Control.Concurrent.Prelude

import           Control.Concurrent.Model
import           Control.Concurrent.Loger
import           Control.Concurrent.Actor.ActorRuntime 
import           Control.Concurrent.Actor.Language
import           Control.Concurrent.Actor.Message
import           Control.Concurrent.Actor.TH

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
        actorId  <- newTextId
        liveFlag <- newMVar True
        threadId <- initActor logerAction actorId chan liveFlag handlerMap 
        pure $ Actor chan threadId actorId liveFlag

    stopRole actor = notify actor StopActor
    killRole actor = killThread (getTreadId actor)

initActor :: Loger -> TextId -> TChan Event -> MVar Bool -> ActorL () -> IO ThreadId
initActor logerAction actorId chan liveFlag handlerMap = forkIO $ do
    actor      <- Actor chan <$> myThreadId <*> pure actorId <*> pure liveFlag
    actRuntime <- newActorRuntime logerAction actor handlerMap
    actorWorker actRuntime actor `finally` setIsDead actor

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

analyzeMessage :: Event -> AnlyzeMessageResult
analyzeMessage message
    | toType message == stopType = StopNodeR
    | otherwise                  = ApplyHandlerR

applyHandler :: ActorRuntimeData -> Event -> IO ()
applyHandler actRuntime message = do
    let messageType = toType message
    case actRuntime ^. handlers . at messageType of
        Just handler -> handler message
        _            -> applyOtherwiseHandler actRuntime message

applyOtherwiseHandler :: ActorRuntimeData -> Event -> IO ()
applyOtherwiseHandler actRuntime message = do
    let printError = (actRuntime ^. loger) handlerNotExistMsg
    maybe printError ($ message) $
        actRuntime ^. handlers . at otherwiseType

handlerNotExistMsg :: Text
handlerNotExistMsg = "[error] handler does not exist, msg is droped."

instance Typeable msg => Listener Actor msg where
    notify actor message = whenM (isLive actor) $
        atomically $ writeTChan (getChan actor) $ toEvent message

data StopActor = StopActor

stopType :: EventType
stopType = toType StopActor