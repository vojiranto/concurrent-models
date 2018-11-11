module Control.StateMachine
    ( StateMachine
    , StateMachineL
    , runStateMachine
    , emit
    , setFinishState
    , addTransition
    , entryDo
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

stateMachineWorker :: IORef R.StateMaschineData -> StateMachine -> IO ()
stateMachineWorker stateMachineRef (StateMachine chan) = do
    machineData <- readIORef stateMachineRef
    event       <- atomically $ readTChan chan
    let currentState = machineData ^. R.currentState
    case R.takeTransition currentState event machineData of
        Nothing       -> stateMachineWorker stateMachineRef (StateMachine chan)
        Just newState -> do
            R.apply currentState (machineData ^. R.exitDo)
            R.apply newState     (machineData ^. R.entryDo)
            modifyIORef stateMachineRef $ R.currentState .~ newState
            if S.member newState (machineData ^. R.finishStates)
                then R.apply newState     (machineData ^. R.exitDo)
                else stateMachineWorker stateMachineRef (StateMachine chan)

runStateMachine :: Typeable a => a -> StateMachineL () -> IO StateMachine
runStateMachine initState machineDescriptione = do
    chan <- atomically newTChan
    void $ forkIO $ do
        stateMachineData <- makeStateMachineData (toMachineState initState) machineDescriptione
        stateMachineRef  <- newIORef stateMachineData
        R.apply (stateMachineData ^. R.currentState) (stateMachineData ^. R.entryDo)
        stateMachineWorker stateMachineRef (StateMachine chan)
    pure $ StateMachine chan

emit :: Typeable a => StateMachine -> a -> IO ()
emit (StateMachine chan) event =
    atomically $ writeTChan chan $ D.toMachineEvent event

