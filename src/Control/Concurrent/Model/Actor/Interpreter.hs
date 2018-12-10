module Control.Concurrent.Model.Actor.Interpreter where

import           Control.Concurrent.Prelude
import qualified Data.Map as M
import           Control.Concurrent.Model.Actor.Language
import           Control.Concurrent.Model.Actor.Message
import           Control.Concurrent.Model.Core

interpretActorL :: Loger -> Actor -> IORef HandlerMap -> ActorF a -> IO a
interpretActorL loger _ m (Math messageType handler next) = do
    loger Trace $ "[add handler] " <> describe messageType
    dataStruct <- readIORef m
    let logTail = describe messageType
    if M.member messageType dataStruct
        then loger Warn  $ "[handler 'math' already exists] " <> logTail
        else loger Trace $ "[set 'math' handler] " <> logTail
    next <$> modifyIORef m (M.insert messageType (toSafe loger messageType handler))

interpretActorL loger _ _ (GetLoger next) = do
    loger Trace "[get loger]"
    pure $ next loger

interpretActorL loger _ _ (ToLog logLevel text next) = do
    loger logLevel text
    pure $ next ()
    
interpretActorL loger link _ (This next) = do
    loger Trace "[get this *]"
    pure $ next link

interpretActorL loger _ _ (LiftIO action next) = do
    loger Trace "[IO action]"
    next <$> action
    

makeHandlerMap :: Loger -> Actor -> ActorL a-> IO HandlerMap
makeHandlerMap loger link h = do
    m <- newIORef mempty
    void $ foldFree (interpretActorL loger link m) h
    readIORef m

toSafe :: Loger -> EventType -> (Event -> IO ()) -> Event -> IO ()
toSafe loger messageType action message = catchAny (action message) $ \ex ->
    loger Warn $ show ex <> " in action with message " <> describe messageType
