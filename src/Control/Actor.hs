module Control.Actor
    ( Actor
    , HandlerL
    , mkActor
    , stopActor
    , killActor
    , notify
    , math
    , otherwiseMath
    , fromActorMessageToType
    , fromDataToMessageType
    ) where

import           Universum
import           Control.Actor.Handler.Language 
import           Control.Actor.Handler.Interpreter
import           Control.Actor.Message
import           Control.Concurrent.STM.TChan
import           Control.Concurrent hiding (MVar, putMVar, takeMVar, newMVar)
import qualified Data.Map           as M

data StopActor = StopActor

stopType :: MessageType
stopType = fromDataToMessageType StopActor

data Actor = Actor (TChan ActorMessage) ThreadId

data AnlyzeMessageResult = StopNodeR | ApplyHandlerR
    deriving Eq

analyzeMessage :: ActorMessage -> AnlyzeMessageResult
analyzeMessage message
    | fromActorMessageToType message == stopType = StopNodeR
    | otherwise                                  = ApplyHandlerR

applyHandler :: HandlerMap -> ActorMessage -> IO ()
applyHandler handlerMap message = do
    let messageType = fromActorMessageToType message
    case messageType `M.lookup` handlerMap of
        Just handler -> handler message
        _            -> whenJust (otherwiseType `M.lookup` handlerMap) $
                            \handler -> handler message

mkActor :: (Actor -> HandlerL a) -> IO Actor
mkActor handler = do
    chan     <- atomically newTChan
    threadId <- forkIO $ do
        actor      <- Actor chan <$> myThreadId
        handlerMap <- makeHandlerMap (handler actor)
        mutex      <- newMVar True
        forever $ do
            void $ takeMVar mutex
            message <- atomically $ readTChan chan
            case analyzeMessage message of
                StopNodeR       -> killActor actor
                ApplyHandlerR   -> do
                    applyHandler handlerMap message
                    putMVar mutex True
    pure $ Actor chan threadId

notify :: Typeable a => Actor -> a -> IO () 
notify (Actor chan _) message = atomically $ writeTChan chan $ toActorMessage message

stopActor :: Actor -> IO ()
stopActor actor = notify actor StopActor

killActor :: Actor -> IO ()
killActor (Actor _ threadId) = killThread threadId