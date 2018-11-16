{-# Language FlexibleInstances     #-}
{-# Language MultiParamTypeClasses #-}

module Control.StateMachine.Interpreter (makeStateMachineData) where

import           Universum
import qualified Data.Map as M
import qualified Data.Set as S
import           Control.Loger
import           Data.Describe
import           Control.Monad.Free
import           Control.StateMachine.Language as L
import           Control.StateMachine.Runtime  as R
import           Control.StateMachine.Domain   as D

makeStateMachineData :: Loger -> D.MachineState -> L.StateMachine -> L.StateMachineL a-> IO R.StateMaschineData
makeStateMachineData logerAction initState stateMachine h = do
    m <- newIORef $ emptyData logerAction initState 
    void $ foldFree (interpretStateMachineL logerAction m stateMachine) h
    readIORef m

interpretStateMachineL :: Loger -> IORef R.StateMaschineData -> L.StateMachine -> L.StateMachineF a -> IO a
interpretStateMachineL toLog _ _ (L.InitialiseAction action next) = do
    toLog "[initialise action]"
    next <$> action

interpretStateMachineL toLog _ link (L.GetMyLink next) = do
    toLog "[get my link]"
    pure $ next link

interpretStateMachineL toLog m _ (L.SetFinishState st next) = do
    toLog $ "[set finish state] " <> describe st
    next <$> modifyIORef m (R.finishStates %~ S.insert st)

interpretStateMachineL toLog m _ (L.AddTransition st1 ev st2 next) = do
    toLog $ "[set transition] " <> describe st1 <> " -> " <> describe ev <> " -> " <> describe st2
    next <$> modifyIORef m (R.transitions %~ M.insert (st1, eventToType ev) st2)    

interpretStateMachineL toLog m _ (L.AddConditionalTransition st1 ev condtition next) = do
    toLog $ "[set conditional transition] " <> describe st1 <> " -> " <> describe ev <> " -> [st] [ ? ]"
    next <$> modifyIORef m (R.conditionalTransitions %~ M.insert (st1, ev) (toSafe toLog ev condtition))

interpretStateMachineL toLog m _ (L.EntryDo st action next) = do
    toLog $ "[set 'entry do' handler] " <> describe st
    next <$> modifyIORef m (R.entryDo %~ M.insert st (toSafeAction toLog action))

interpretStateMachineL toLog m _ (L.TransitionDo st1 st2 eventType action next) = do
    toLog $ "[set 'transition do' handler] " <> describe st1 <> " -> " <> describe eventType <> " -> " <> describe st2
    next <$> modifyIORef m (R.transitionDo %~ M.insert (st1, eventType, st2) (toSafe toLog eventType action))    

interpretStateMachineL toLog m _ (L.EntryWithEventDo st1 eventType action next) = do
    toLog $ "[set 'entry with event do' handler] " <> describe st1 <> " " <> describe eventType
    next <$> modifyIORef m (R.entryWithEventDo %~ M.insert (st1, eventType) (toSafe toLog eventType action))

interpretStateMachineL toLog m _ (L.StaticalDo st1 eventType action next) = do
    toLog $ "[set 'statical do' handler] " <> describe st1 <> " " <> describe eventType
    next <$> modifyIORef m (R.staticalDo %~ M.insert (st1, eventType) (toSafe toLog eventType  action))

interpretStateMachineL toLog m _ (L.ExitWithEventDo st1 eventType action next) = do
    toLog $ "[set 'exit with event do' handler] " <> describe st1 <> " " <> describe eventType
    next <$> modifyIORef m (R.exitWithEventDo %~ M.insert (st1, eventType) (toSafe toLog eventType action))    

interpretStateMachineL toLog m _ (L.ExitDo st action next) = do
    toLog $ "[set 'exit do' handler] " <> describe st
    next <$> modifyIORef m (R.exitDo %~ M.insert st (toSafeAction toLog action))

class ToSafe t a where
    toSafe :: (Text -> IO ()) -> t -> a -> a

instance ToSafe EventType (MachineEvent -> IO ()) where
    toSafe loger' eventType action event = catchAny (action event) $ \ex ->
        loger' $ "[error] " <> show ex <> " in action with event " <> describe eventType

instance ToSafe EventType (MachineEvent -> IO (Maybe MachineState)) where
    toSafe loger' eventType condition event = catchAny (condition event) $ \ex -> do
        loger' $ "[error] " <> show ex <> " in condition with event " <> describe eventType
        pure Nothing

toSafeAction :: (Text -> IO ()) -> IO () -> IO () 
toSafeAction loger' action = catchAny action $ \ex -> loger' $ "[error] " <> show ex <> " in action."
