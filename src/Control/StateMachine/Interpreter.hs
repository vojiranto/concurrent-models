{-# Language FlexibleInstances     #-}
{-# Language MultiParamTypeClasses #-}

module Control.StateMachine.Interpreter where

import           Universum hiding (ToText (..))
import qualified Data.Map as M
import qualified Data.Set as S
import           Control.Monad.Free
import           Control.StateMachine.Language as L
import           Control.StateMachine.Runtime  as R
import           Control.StateMachine.Domain   as D

interpretStateMachineL :: (Text -> IO ()) -> IORef R.StateMaschineData -> L.StateMachine -> L.StateMachineF a -> IO a
interpretStateMachineL _ _ _ (L.InitialiseAction action next) = next <$> action

interpretStateMachineL _ _ link (L.MyLink next) = pure $ next link

interpretStateMachineL _ m _ (L.SetFinishState st next) =
    next <$> modifyIORef m (R.finishStates %~ S.insert st)

interpretStateMachineL _ m _ (L.AddTransition st1 ev st2 next) =
    next <$> modifyIORef m (R.transitions %~ M.insert (st1, eventToType ev) st2)    

interpretStateMachineL loger m _ (L.AddConditionalTransition st1 ev condtition next) =
    next <$> modifyIORef m (R.conditionalTransitions %~ M.insert (st1, ev) (toSafe loger ev condtition))

interpretStateMachineL loger m _ (L.EntryDo st action next) =
    next <$> modifyIORef m (R.entryDo %~ M.insert st (toSafeAction loger action))

interpretStateMachineL loger m _ (L.TransitionDo st1 st2 eventType action next) =
    next <$> modifyIORef m (R.transitionDo %~ M.insert (st1, eventType, st2) (toSafe loger eventType action))    

interpretStateMachineL loger m _ (L.EntryWithEventDo st1 eventType action next) =
    next <$> modifyIORef m (R.entryWithEventDo %~ M.insert (st1, eventType) (toSafe loger eventType action))

interpretStateMachineL loger m _ (L.StaticalDo st1 eventType action next) =
    next <$> modifyIORef m (R.staticalDo %~ M.insert (st1, eventType) (toSafe loger eventType  action))

interpretStateMachineL loger m _ (L.ExitWithEventDo st1  eventType action next) =
    next <$> modifyIORef m (R.exitWithEventDo %~ M.insert (st1, eventType) (toSafe loger eventType action))    

interpretStateMachineL loger m _ (L.ExitDo st action next) =
    next <$> modifyIORef m (R.exitDo %~ M.insert st (toSafeAction loger action))

makeStateMachineData :: (Text -> IO ()) -> D.MachineState -> L.StateMachine -> L.StateMachineL a-> IO R.StateMaschineData
makeStateMachineData loger initState stateMachine h = do
    m <- newIORef $ emptyData initState
    void $ foldFree (interpretStateMachineL loger m stateMachine) h
    readIORef m

class ToSafe t a where
    toSafe :: (Text -> IO ()) -> t -> a -> a

instance ToSafe EventType (MachineEvent -> IO ()) where
    toSafe loger eventType action event = catchAny (action event) $ \ex ->
        loger $ "[error] " <> show ex <> " in action with event " <> toText eventType

instance ToSafe EventType (MachineEvent -> IO (Maybe MachineState)) where
    toSafe loger eventType condition event = catchAny (condition event) $ \ex -> do
        loger $ "[error] " <> show ex <> " in condition with event " <> toText eventType
        pure Nothing

toSafeAction :: (Text -> IO ()) -> IO () -> IO () 
toSafeAction loger action = catchAny action $ \ex -> loger $ "[error] " <> show ex <> " in action."
    