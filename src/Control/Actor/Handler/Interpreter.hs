module Control.Actor.Handler.Interpreter where

import           Universum
import qualified Data.Map as M
import           Control.Monad.Free
import           Control.Actor.Handler.Language
import           Control.Actor.Message

type HandlerMap = M.Map MsgType (ActorMsg -> IO ())

interpretHandlerL :: IORef HandlerMap -> HandlerF a -> IO a
interpretHandlerL m (MakeHandler msgType handler next) = do
    modifyIORef m (M.insert msgType handler)
    pure (next ())

runHandlerL :: HandlerL a-> IO HandlerMap
runHandlerL h = do
    m <- newIORef mempty
    foldFree (interpretHandlerL m) h
    readIORef m