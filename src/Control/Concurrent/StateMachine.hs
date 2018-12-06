{-# Language ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Control.Concurrent.StateMachine
    ( 
      StateMachine
    , StateMachineL
    , MachineState
    , HaveTextId(..)
    , Describe(..)
    , Math(..)
    , Fsm (..)
    , TextId
    , This(..)
    , runStateMachine
    -- * Making of FSM struct
    , addFinalState
    , groupStates
    , (<:)
    , addTransition
    , ifE
    , (>->)
    , addConditionalTransition
    , (>?>)
    , just
    , nothing
    , is
    -- * Addition of handlers
    , L.mathS
    , Acception (..)
    -- * Communication with working FSN
    , Listener (..)
    -- * Templates
    , makeStates
    , makeEvents
    , makeFsm
    ) where

import           Universum
import           Control.Concurrent (forkIO)
import           Control.Concurrent.Chan

import           Control.Concurrent.Flag
import           Control.Concurrent.Loger
import           Control.Concurrent.Core
import           Control.Concurrent.StateMachine.TH
import           Control.Concurrent.StateMachine.Language                      as L
import           Control.Concurrent.StateMachine.Interpreter                   as I
import qualified Control.Concurrent.StateMachine.Runtime                       as R
import           Control.Concurrent.StateMachine.Runtime.StateMaschineHandlers as R
import           Control.Concurrent.StateMachine.Runtime.StateMaschineStruct   as R 
import           Control.Concurrent.StateMachine.Domain                        as D

-- | Emit event to the FSN.
instance Typeable msg => Listener StateMachine msg where
    notify fsm event = whenM (isLive fsm) $
        writeChan (getEventVar fsm) . D.FastEvent $ toEvent event
    
    notifyAndWait fsm event = whenM (isLive fsm) $ do
        processed <- newFlag
        writeChan (getEventVar fsm) $ D.WaitEvent (toEvent event) processed
        wait processed

class Fsm fsm where
    -- | Build and run new state machine, interrupts the build if an error is
    --   detected in the machine description.
    runFsm    :: Typeable a => Loger -> a -> StateMachineL () -> IO fsm
    -- | Take current state of the FSN.
    readState :: fsm -> IO MachineState

runStateMachine :: Typeable a => Loger -> a -> StateMachineL () -> IO StateMachine
runStateMachine = runFsm

instance Fsm StateMachine where
    runFsm logerAction (toMachineState -> initState) machineDescriptione = do
        fsmRef <- newFsmRef initState  
        initFsm logerAction fsmRef machineDescriptione
        pure fsmRef

    readState = takeState

newFsmRef :: MachineState -> IO StateMachine
newFsmRef initState =
    StateMachine <$> newChan <*> newMVar initState <*> newTextId <*> newMVar True

initFsm :: Loger -> StateMachine -> StateMachineL a -> IO ()
initFsm logerAction fsmRef machineDescriptione = void $ forkIO $ do
    loger <- addTagToLoger logerAction "[SM]" (getTextId fsmRef)
    eFsm  <- makeStateMachineData loger fsmRef machineDescriptione
    either (printError loger) (startFsm fsmRef) eFsm `finally` setIsDead fsmRef

printError :: Show a => Loger -> a -> IO ()
printError loger err = loger $ "[error] " <> show err

startFsm :: StateMachine -> IORef R.StateMaschineData -> IO ()
startFsm fsmRef fsmDataRef = do 
    entryInitState     fsmDataRef
    stateMachineWorker fsmDataRef fsmRef

entryInitState :: IORef R.StateMaschineData -> IO ()
entryInitState fsmDataRef = do
    fsmData <- readIORef fsmDataRef
    fsmData ^. R.loger $ "[init state] " <> describe (fsmData ^. R.currentState)
    let stList = R.takeGroups (fsmData ^. R.stateMachineStruct) (fsmData ^. R.currentState)
    forM_ stList (R.applyEntryDo (fsmData ^. R.loger) (fsmData ^. R.handlers))


eventAnalize, stateAnalize, stateMachineWorker
    :: IORef R.StateMaschineData -> StateMachine -> IO ()

stateMachineWorker = stateAnalize

stateAnalize stateMachineRef stateMachine = do
    machineData <- readIORef stateMachineRef
    if R.isFinish machineData
        then do
            let currentState = machineData ^. R.currentState
            machineData ^. R.loger $ "[finish state] " <> describe currentState
            let stList = R.takeGroups
                    (machineData ^. R.stateMachineStruct)
                    (machineData ^. R.currentState)
            forM_ stList (R.applyExitDo (machineData ^. R.loger) (machineData ^. R.handlers))
        else eventAnalize stateMachineRef stateMachine

eventAnalize stateMachineRef fsmRef@(StateMachine events _ _ _) = do
    (event, processed) <- getMachineEvent =<< readChan events

    machineData        <- readIORef stateMachineRef
    machineData ^. R.loger $ describe event
    applyStatic machineData event
    applyMath (machineData ^. R.loger) (machineData ^. R.handlers) event
    whenJustM (R.takeTransition event machineData)
        (applyTransition stateMachineRef event fsmRef)

    whenJust processed liftFlag

    stateAnalize stateMachineRef fsmRef

applyStatic :: R.StateMaschineData -> Event -> IO ()
applyStatic machineData event = do
    let stList = R.takeGroups
            (machineData ^. R.stateMachineStruct)
            (machineData ^. R.currentState)
    forM_ stList $ \st ->
        R.applyStaticalDo (machineData ^. R.loger) (machineData ^. R.handlers) st event

applyTransition
    :: IORef R.StateMaschineData -> Event -> StateMachine -> Transition -> IO ()
applyTransition stateMachineRef event fsmRef (D.Transition currentState newState) = do
    fsmData <- readIORef stateMachineRef
    fsmData ^. R.loger $ showTransition currentState event newState
    changeState stateMachineRef fsmRef newState
    R.applyTransitionActions fsmData currentState event newState

showTransition :: MachineState -> Event -> MachineState -> Text
showTransition st1 ev st2 =
    "[transition] " <> describe st1 <> " -> " <> describe ev <> " -> " <> describe st2

changeState :: IORef R.StateMaschineData -> StateMachine -> MachineState -> IO ()
changeState stateMachineRef fsm newState = do
    void $ swapMVar (getStateVar fsm) newState
    modifyIORef stateMachineRef $ R.currentState .~ newState