module Control.Actor
    ( Actor
    , HandlerL
    , mkActor
    , stopActor
    , notify
    , math
    , otherwiseMath
    , fromActorMsgToType
    , fromDataToMsgType
    ) where

import           Universum
import           Control.Handler.Language 
import           Control.Handler.Interpreter
import           Control.ActorMsg
import           Control.Concurrent.STM.TChan
import           Control.Concurrent
import qualified Data.Map           as M

data StopActor = StopActor

newtype Actor = Actor (TChan ActorMsg)

mkActor :: (Actor -> HandlerL a) -> IO (Actor, IO ())
mkActor handler = do
    chan <- atomically newTChan
    let actor = Actor chan
    handlerMap <- runHandlerL (handler actor)
    let loop = do
            msg <- atomically (readTChan chan)
            let msgType = fromActorMsgToType msg
            if msgType == fromDataToMsgType StopActor then pure ()
            else do
                case msgType `M.lookup` handlerMap of
                    Just handle -> handle msg
                    Nothing     -> case otherwiseType `M.lookup` handlerMap of
                        Just handle -> handle msg
                        Nothing     -> error "Msg with type is droped."
                loop
    void $ forkIO loop 
    pure (actor, stopActor actor)

notify :: Typeable a => Actor -> a -> IO () 
notify (Actor chan) msg = atomically $ writeTChan chan $ toActorMsg msg

stopActor :: Actor -> IO ()
stopActor actor = notify actor StopActor
