module Control.Concurrent.Actor
    ( Actor
    , ActorL
    , runActor
    , stopActor
    , killActor
    , notify
    , this
    , math
    , otherwiseMath
    , fromActorMessage
    , ToType(..)
    ) where

import           Universum
import           Data.TextId
import           Data.This
import           Data.Describe
import           Control.Concurrent.Loger
import           Control.Concurrent.Actor.Language 
import           Control.Concurrent.Actor.Interpreter
import           Control.Concurrent.Actor.Message
import           Control.Concurrent.STM.TChan
import           Control.Concurrent hiding (MVar, putMVar, takeMVar, newMVar)
import qualified Data.Map           as M

data StopActor = StopActor

stopType :: MessageType
stopType = toType StopActor

data AnlyzeMessageResult = StopNodeR | ApplyHandlerR
    deriving Eq

analyzeMessage :: ActorMessage -> AnlyzeMessageResult
analyzeMessage message
    | toType message == stopType = StopNodeR
    | otherwise                  = ApplyHandlerR

applyHandler :: Loger -> HandlerMap -> ActorMessage -> IO ()
applyHandler loger handlerMap message = do
    let messageType = toType message
    case messageType `M.lookup` handlerMap of
        Just handler -> handler message
        _            -> do
            let mHandler = otherwiseType `M.lookup` handlerMap 
            whenJust mHandler $ \handler -> handler message
            unless (isJust mHandler) $ loger "[error] handler does not exist, msg is droped." 

-- | Build and run new actor.
runActor :: Loger -> ActorL a -> IO Actor
runActor logerAction handler = do
    chan     <- atomically newTChan
    threadId <- forkIO $ do
        textId          <- newTextId
        let loger txt   = logerAction $ "[Actor] " <> "[" <> textId  <> "] " <> txt
        actor           <- Actor chan <$> myThreadId
        handlerMap      <- makeHandlerMap loger actor handler
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


-- | Send msg to the actor.
notify :: Typeable a => Actor -> a -> IO () 
notify (Actor chan _) message = atomically $ writeTChan chan $ toActorMessage message

-- | Send stop msg to the actor.
stopActor :: Actor -> IO ()
stopActor actor = notify actor StopActor

-- | Send async exeption to the actor.
killActor :: Actor -> IO ()
killActor (Actor _ threadId) = killThread threadId