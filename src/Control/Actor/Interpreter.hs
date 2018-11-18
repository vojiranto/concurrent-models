module Control.Actor.Interpreter where

import           Universum
import qualified Data.Map as M
import           Control.Loger
import           Control.Monad.Free
import           Control.Actor.Language
import           Control.Actor.Message
import           Data.Describe

type HandlerMap = M.Map MessageType (ActorMessage -> IO ())

interpretActorL :: Loger -> IORef HandlerMap -> ActorF a -> IO a
interpretActorL loger m (Math messageType handler next) = do
    loger $ "[add handler] " <> describe messageType
    next <$> modifyIORef m (M.insert messageType (toSafe loger messageType handler))

makeHandlerMap :: Loger -> ActorL a-> IO HandlerMap
makeHandlerMap loger h = do
    m <- newIORef mempty
    void $ foldFree (interpretActorL loger m) h
    readIORef m

toSafe :: Loger -> MessageType -> (ActorMessage -> IO ()) -> ActorMessage -> IO ()
toSafe loger messageType action message = catchAny (action message) $ \ex ->
    loger $ "[error] " <> show ex <> " in action with message " <> describe messageType
