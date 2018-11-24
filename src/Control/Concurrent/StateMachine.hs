{-# Language ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Control.Concurrent.StateMachine
    ( 
      StateMachine
    , StateMachineL
    , runStateMachine
    -- * Making of FSM struct
    , this
    , addFinalState
    , groupStates
    , (<:)
    , addTransition
    , addConditionalTransition
    , just
    , nothing
    , is
    -- * Addition of handlers
    , L.staticalDo
    , L.exitDo
    , L.exitWithEventDo
    , L.entryWithEventDo
    , L.entryDo
    -- * Communication with working FSN
    , Listener (..)
    , takeState
    -- * Templates
    , makeStates
    , makeEvents
    ) where

import           Universum
import           Data.Flag
import           Data.Describe
import           Control.Concurrent.Loger
import           Control.Concurrent (forkIO)
import           Control.Concurrent.Chan

import           Control.Concurrent.Listener
import           Control.Concurrent.StateMachine.TH
import           Control.Concurrent.StateMachine.Language                      as L
import           Control.Concurrent.StateMachine.Interpreter                   as I
import qualified Control.Concurrent.StateMachine.Runtime                       as R
import           Control.Concurrent.StateMachine.Runtime.StateMaschineHandlers as R
import           Control.Concurrent.StateMachine.Runtime.StateMaschineStruct   as R 
import           Control.Concurrent.StateMachine.Domain                        as D

eventAnalize, stateAnalize, stateMachineWorker
    :: IORef R.StateMaschineData -> StateMachine -> IO ()
eventAnalize stateMachineRef (StateMachine eventVar stateVar) = do
    (event, processed) <- getEvent =<< readChan eventVar
    machineData        <- readIORef stateMachineRef

    machineData ^. R.loger $ describe event
    let stList = R.takeGroups
            (machineData ^. R.stateMachineStruct)
            (machineData ^. R.currentState)
    forM_ stList $ \st ->
        R.applyStaticalDo (machineData ^. R.loger) (machineData ^. R.handlers) st event

    mTransition        <- R.takeTransition event machineData

    whenJust mTransition $ \(D.Transition currentState newState) -> do
        machineData ^. R.loger $ showTransition currentState event newState
        void $ swapMVar stateVar newState
        modifyIORef stateMachineRef $ R.currentState .~ newState
        R.applyTransitionActions machineData currentState event newState

    whenJust processed liftFlag
    stateAnalize stateMachineRef (StateMachine eventVar stateVar)

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

showTransition :: MachineState -> MachineEvent -> MachineState -> Text
showTransition st1 ev st2 =
    "[transition] " <> describe st1 <> " -> " <> describe ev <> " -> " <> describe st2
stateMachineWorker = stateAnalize


-- | Build and run new state machine, interrupts the build if an error is
--   detected in the machine description.
runStateMachine :: Typeable a => Loger -> a -> StateMachineL () -> IO StateMachine
runStateMachine logerAction (toMachineState -> initState) machineDescriptione = do
    fsmRef <- newFsmRef initState  
    initFsm logerAction fsmRef machineDescriptione
    pure fsmRef

initFsm :: Loger -> StateMachine -> StateMachineL a -> IO ()
initFsm logerAction fsmRef machineDescriptione = void $ forkIO $ do
    loger     <- addTagToLoger logerAction "[SM]"
    initState <- takeState fsmRef
    mStateMachineData <- makeStateMachineData loger initState fsmRef machineDescriptione
    case mStateMachineData of
        Right fsmDataRef -> startFsm fsmRef fsmDataRef 
        Left err -> printError loger err


startFsm :: StateMachine -> IORef R.StateMaschineData -> IO ()
startFsm fsmRef fsmDataRef = do 
    entryInitState fsmDataRef
    stateMachineWorker fsmDataRef fsmRef

printError :: Show a => Loger -> a -> IO ()
printError loger err = loger $ "[error] " <> show err

newFsmRef :: MachineState -> IO StateMachine
newFsmRef initState = StateMachine <$> newChan <*> newMVar initState

entryInitState :: IORef R.StateMaschineData -> IO ()
entryInitState fsmDataRef = do
    fsmData <- readIORef fsmDataRef
    fsmData ^. R.loger $ "[init state] " <> describe (fsmData ^. R.currentState)
    let stList = R.takeGroups (fsmData ^. R.stateMachineStruct) (fsmData ^. R.currentState)
    forM_ stList (R.applyEntryDo (fsmData ^. R.loger) (fsmData ^. R.handlers))


-- | Emit event to the FSN.
instance Typeable msg => Listener StateMachine msg where
    notify (StateMachine eventVar _) = writeChan eventVar . D.FastEvent . D.toMachineEvent
    notifyAndWait (StateMachine eventVar _) event = do
        processed <- newFlag
        writeChan eventVar $ D.WaitEvent (D.toMachineEvent event) processed
        wait processed


-- | Take current state of the FSN.
takeState :: StateMachine -> IO MachineState
takeState (StateMachine _ stateVar) = readMVar stateVar
