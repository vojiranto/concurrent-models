module Control.Actor.Handler.Interpreter where

import           Universum
import qualified Data.Map as M
import           Control.Loger
import           Control.Monad.Free
import           Control.Actor.Handler.Language
import           Control.Actor.Message
import           Data.Describe

type HandlerMap = M.Map MessageType (ActorMessage -> IO ())

interpretHandlerL :: Loger -> IORef HandlerMap -> HandlerF a -> IO a
interpretHandlerL loger m (MakeHandler messageType handler next) = do
    loger $ "[add handler] " <> describe messageType
    next <$> modifyIORef m (M.insert messageType (toSafe loger messageType handler))

makeHandlerMap :: Loger -> HandlerL a-> IO HandlerMap
makeHandlerMap loger h = do
    m <- newIORef mempty
    void $ foldFree (interpretHandlerL loger m) h
    readIORef m

toSafe :: Loger -> MessageType -> (ActorMessage -> IO ()) -> ActorMessage -> IO ()
toSafe loger messageType action message = catchAny (action message) $ \ex ->
    loger $ "[error] " <> show ex <> " in action with message " <> describe messageType
