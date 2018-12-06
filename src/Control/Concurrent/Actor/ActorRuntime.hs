{-# Language TemplateHaskell #-}
module Control.Concurrent.Actor.ActorRuntime where

import           Control.Concurrent.Prelude

import           Control.Concurrent.Model.Data.TextId
import           Control.Concurrent.Loger
import           Control.Concurrent.Actor.Language
import           Control.Concurrent.Actor.Interpreter 

data ActorRuntimeData = ActorRuntimeData
    { _loger    :: Loger
    , _handlers :: HandlerMap
    }
makeLenses ''ActorRuntimeData

newActorRuntime :: Loger -> Actor -> ActorL () -> IO ActorRuntimeData
newActorRuntime logerAction actor handlers' = do
    loger'     <- addTagToLoger logerAction "[Actor]" (getTextId actor)
    handlerMap <- makeHandlerMap loger' actor handlers'
    pure $ ActorRuntimeData loger' handlerMap