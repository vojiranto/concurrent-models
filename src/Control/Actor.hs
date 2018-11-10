module Control.Actor
    ( Actor
    , HandlerL
    , mkActor
    , stopActor
    , killActor
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

data Actor = Actor (TChan ActorMsg) ThreadId

mkActor :: (Actor -> HandlerL a) -> IO Actor
mkActor handler = do
    chan <- atomically newTChan
    threadId <- forkIO $ do
        threadId   <- myThreadId
        handlerMap <- runHandlerL (handler $ Actor chan threadId)
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
        loop
    pure $ Actor chan threadId

notify :: Typeable a => Actor -> a -> IO () 
notify (Actor chan _) msg = atomically $ writeTChan chan $ toActorMsg msg

stopActor :: Actor -> IO ()
stopActor actor = notify actor StopActor

killActor :: Actor -> IO ()
killActor (Actor _ threadId) = killThread threadId