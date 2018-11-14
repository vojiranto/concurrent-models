module Control.StateMachine
    ( StateMachine
    , StateMachineL
    , runStateMachine
    , emit
    , setFinishState
    , addTransition
    , entryDo
    , transitionDo
    , exitDo
    ) where

import           Universum

import           Control.Concurrent (forkIO)
import           Control.Concurrent.STM.TChan
import qualified Data.Set as S

import           Control.StateMachine.Language      as L
import           Control.StateMachine.Interpreter   as I
import qualified Control.StateMachine.Runtime       as R
import           Control.StateMachine.Domain        as D

newtype StateMachine = StateMachine (TChan D.MachineEvent)

eventAnalize, stateAnalize, stateMachineWorker :: IORef R.StateMaschineData -> StateMachine -> IO ()
eventAnalize stateMachineRef (StateMachine chan) = do
    event       <- atomically $ readTChan chan
    machineData <- readIORef stateMachineRef
    whenJust (R.takeTransition event machineData) $
        \(R.Transition currentState newState) -> do
            R.applyExitAction       machineData currentState
            R.applyExitWithEventDo  machineData currentState event
            R.applyTransition       machineData currentState event newState
            R.applyEntryWithEventDo machineData newState event
            R.applyEntryAction      machineData newState
            modifyIORef stateMachineRef $ R.currentState .~ newState
    stateAnalize stateMachineRef (StateMachine chan)

stateAnalize stateMachineRef (StateMachine chan) = do
    machineData <- readIORef stateMachineRef
    let currentState = machineData ^. R.currentState
    if R.isFinish machineData currentState
        then R.applyExitAction machineData currentState
        else eventAnalize stateMachineRef (StateMachine chan)

stateMachineWorker = stateAnalize

runStateMachine :: Typeable a => a -> StateMachineL () -> IO StateMachine
runStateMachine initState machineDescriptione = do
    chan <- atomically newTChan
    void $ forkIO $ do
        stateMachineData <- makeStateMachineData (toMachineState initState) machineDescriptione
        stateMachineRef  <- newIORef stateMachineData
        R.applyEntryAction stateMachineData (stateMachineData ^. R.currentState)
        stateMachineWorker stateMachineRef  (StateMachine chan)
    pure $ StateMachine chan

emit :: Typeable a => StateMachine -> a -> IO ()
emit (StateMachine chan) event =
    atomically $ writeTChan chan $ D.toMachineEvent event

