module Control.StateMachine.Interpreter where

import           Universum
import qualified Data.Map as M
import qualified Data.Set as S
import           Control.Monad.Free
import           Control.StateMachine.Language as L
import           Control.StateMachine.Runtime  as R
import           Control.StateMachine.Domain   as D

interpretStateMachineL :: IORef R.StateMaschineData -> L.StateMachineF a -> IO a
interpretStateMachineL m (L.SetFinishState st next) =
    next <$> modifyIORef m (R.finishStates %~ S.insert st)
interpretStateMachineL m (L.AddTransition st1 ev st2 next) =
    next <$> modifyIORef m (R.transitions %~ R.addTransitionToMap st1 ev st2 )
interpretStateMachineL m (L.EntryDo st action next) =
    next <$> modifyIORef m (R.entryDo %~ M.insert st action)
interpretStateMachineL m (L.TransitionDo st1 st2 eventType action next) =
    next <$> modifyIORef m (R.transitionDo %~ M.insert (st1, eventType, st2) action)    
interpretStateMachineL m (L.ExitDo st action next) =
    next <$> modifyIORef m (R.exitDo %~ M.insert st action)

makeStateMachineData :: D.MachineState -> L.StateMachineL a-> IO R.StateMaschineData
makeStateMachineData initState h = do
    m <- newIORef $ emptyData initState
    foldFree (interpretStateMachineL m) h
    readIORef m