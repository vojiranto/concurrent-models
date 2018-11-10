module Control.Actor.Handler.Interpreter where

import           Universum
import qualified Data.Map as M
import           Control.Monad.Free
import           Control.Actor.Handler.Language
import           Control.Actor.Message

type HandlerMap = M.Map MessageType (ActorMessage -> IO ())

interpretHandlerL :: IORef HandlerMap -> HandlerF a -> IO a
interpretHandlerL m (MakeHandler messageType handler next) = do
    modifyIORef m (M.insert messageType handler)
    pure (next ())

makeHandlerMap :: HandlerL a-> IO HandlerMap
makeHandlerMap h = do
    m <- newIORef mempty
    foldFree (interpretHandlerL m) h
    readIORef m