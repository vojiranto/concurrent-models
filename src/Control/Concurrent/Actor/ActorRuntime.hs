{-# Language TemplateHaskell #-}
module Control.Concurrent.Actor.ActorRuntime where

import           Universum
import           Data.Mutex
import           Control.Lens.TH
import           Control.Concurrent.Loger
import           Control.Concurrent.Actor.Language
import           Control.Concurrent.Actor.Interpreter 

data ActorRuntimeData = ActorRuntimeData
    { _mutex    :: Mutex
    , _loger    :: Loger
    , _handlers :: HandlerMap
    }
makeLenses ''ActorRuntimeData

newActorRuntime :: Loger -> Actor -> ActorL () -> IO ActorRuntimeData
newActorRuntime logerAction actor handlers' = do
    mutex'     <- newMutex
    loger'     <- addTagToLoger logerAction "[Actor]"
    handlerMap <- makeHandlerMap loger' actor handlers'
    pure $ ActorRuntimeData mutex' loger' handlerMap