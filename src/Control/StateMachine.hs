{-# Language ViewPatterns #-}
module Control.StateMachine
    ( StateMachine
    , StateMachineL
    , runStateMachine
    , initialiseAction
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
import qualified Data.Set as S
import qualified Data.Map as M

import           Control.StateMachine.Language      as L
import           Control.StateMachine.Interpreter   as I
import qualified Control.StateMachine.Runtime       as R
import           Control.StateMachine.Domain        as D

newtype StateMachine = StateMachine (MVar D.MachineEvent)

eventAnalize, stateAnalize, stateMachineWorker :: IORef R.StateMaschineData -> StateMachine -> IO ()
eventAnalize stateMachineRef (StateMachine eventVar) = do
    event       <- takeMVar  eventVar
    machineData <- readIORef stateMachineRef

    mTransition <- R.takeTransition event machineData
    unless (isJust mTransition) $
        R.applyEvent (machineData ^. R.currentState) event (machineData ^. R.staticalDo)
    whenJust mTransition $ \(R.Transition currentState newState) -> do
        R.applyTransitionActions machineData currentState event newState
        modifyIORef stateMachineRef $ R.currentState .~ newState
    stateAnalize stateMachineRef (StateMachine eventVar)

stateAnalize stateMachineRef stateMachine = do
    machineData <- readIORef stateMachineRef
    let currentState = machineData ^. R.currentState
    if R.isFinish machineData currentState
        then R.apply currentState (machineData ^. R.exitDo)
        else eventAnalize stateMachineRef stateMachine

stateMachineWorker = stateAnalize

runStateMachine :: Typeable a => a -> StateMachineL () -> IO StateMachine
runStateMachine (toMachineState -> initState) machineDescriptione = do
    eventVar <- newEmptyMVar
    let stateMachine = StateMachine eventVar
    void $ forkIO $ do
        stateMachineData <- makeStateMachineData initState machineDescriptione
        stateMachineRef  <- newIORef stateMachineData
        R.apply initState (stateMachineData ^. R.entryDo)
        stateMachineWorker stateMachineRef stateMachine
    pure stateMachine

emit :: Typeable a => StateMachine -> a -> IO ()
emit (StateMachine eventVar) = putMVar eventVar . D.toMachineEvent

