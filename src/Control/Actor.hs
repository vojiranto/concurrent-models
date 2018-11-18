module Control.Actor
    ( Actor
    , ActorL
    , makeActor
    , stopActor
    , killActor
    , notify
    , math
    , otherwiseMath
    , fromActorMessageToType
    , fromDataToMessageType
    ) where

import           Universum
import           Data.TextId
import           Data.Describe
import           Control.Loger
import           Control.Actor.Language 
import           Control.Actor.Interpreter
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

applyHandler :: Loger -> HandlerMap -> ActorMessage -> IO ()
applyHandler loger handlerMap message = do
    let messageType = fromActorMessageToType message
    case messageType `M.lookup` handlerMap of
        Just handler -> handler message
        _            -> do
            let mHandler = otherwiseType `M.lookup` handlerMap 
            whenJust mHandler $ \handler -> handler message
            unless (isJust mHandler) $ loger "[error] handler does not exist, msg is droped." 

makeActor :: Loger -> (Actor -> ActorL a) -> IO Actor
makeActor logerAction handler = do
    chan     <- atomically newTChan
    threadId <- forkIO $ do
        textId          <- newTextId
        let loger txt   = logerAction $ "[Actor] " <> "[" <> textId  <> "] " <> txt
        actor           <- Actor chan <$> myThreadId
        handlerMap      <- makeHandlerMap loger (handler actor)
        mutex           <- newMVar True
        forever $ do
            void $ takeMVar mutex
            message <- atomically $ readTChan chan
            loger $ "[message] " <> describe message
            case analyzeMessage message of
                StopNodeR       -> killActor actor
                ApplyHandlerR   -> do
                    applyHandler loger handlerMap message
                    putMVar mutex True
    pure $ Actor chan threadId

notify :: Typeable a => Actor -> a -> IO () 
notify (Actor chan _) message = atomically $ writeTChan chan $ toActorMessage message

stopActor :: Actor -> IO ()
stopActor actor = notify actor StopActor

killActor :: Actor -> IO ()
killActor (Actor _ threadId) = killThread threadId