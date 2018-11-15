{-# Language ViewPatterns #-}
module Control.StateMachine
    ( StateMachine
    , StateMachineL
    , runStateMachine
    , setFinishState
    , addTransition
    , addConditionalTransition
    , staticalDo
    , exitDo
    , exitWithEventDo
    , transitionDo
    , entryWithEventDo
    , entryDo
    , emit
    , just
    , nothing
    ) where

import           Universum

import           Control.Concurrent (forkIO)
import           Control.Concurrent.STM.TChan
import qualified Data.Set as S
import qualified Data.Map as M

import           Control.StateMachine.Language      as L
import           Control.StateMachine.Interpreter   as I
import qualified Control.StateMachine.Runtime       as R
import           Control.StateMachine.Domain        as D

newtype StateMachine = StateMachine (TChan D.MachineEvent)

eventAnalize, stateAnalize, stateMachineWorker :: IORef R.StateMaschineData -> StateMachine -> IO ()
eventAnalize stateMachineRef (StateMachine chan) = do
    event       <- atomically $ readTChan chan
    machineData <- readIORef stateMachineRef

    mTransition <- R.takeTransition event machineData
    unless (isJust mTransition) $
        R.applyEvent (machineData ^. R.currentState) event (machineData ^. R.staticalDo)
    whenJust mTransition $ \(R.Transition currentState newState) -> do
        R.applyTransitionActions machineData currentState event newState
        modifyIORef stateMachineRef $ R.currentState .~ newState
    stateAnalize stateMachineRef (StateMachine chan)

stateAnalize stateMachineRef (StateMachine chan) = do
    machineData <- readIORef stateMachineRef
    let currentState = machineData ^. R.currentState
    if R.isFinish machineData currentState
        then R.apply currentState (machineData ^. R.exitDo)
        else eventAnalize stateMachineRef (StateMachine chan)

stateMachineWorker = stateAnalize

runStateMachine :: Typeable a => a -> StateMachineL () -> IO StateMachine
runStateMachine (toMachineState -> initState) machineDescriptione = do
    chan <- atomically newTChan
    void $ forkIO $ do
        stateMachineData <- makeStateMachineData initState machineDescriptione
        stateMachineRef  <- newIORef stateMachineData
        R.apply initState (stateMachineData ^. R.entryDo)
        stateMachineWorker stateMachineRef  (StateMachine chan)
    pure $ StateMachine chan

emit :: Typeable a => StateMachine -> a -> IO ()
emit (StateMachine chan) event =
    atomically $ writeTChan chan $ D.toMachineEvent event

