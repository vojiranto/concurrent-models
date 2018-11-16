{-# Language ViewPatterns #-}
module Control.StateMachine
    ( StateMachine
    , StateMachineL
    , makeStates
    , makeEvents
    , runStateMachine
    , initialiseAction
    , setFinishState
    , getMyLink
    , addTransition
    , addConditionalTransition
    , staticalDo
    , exitDo
    , exitWithEventDo
    , transitionDo
    , entryWithEventDo
    , entryDo
    , emit
    , emitAndWait
    , just
    , nothing
    , takeState
    , is
    , (<:)
    ) where

import           Universum
import           Data.TextId
import           Data.Describe
import           Control.Loger
import           Control.Concurrent (forkIO)
import           Control.Concurrent.Chan

import           Control.StateMachine.TH
import           Control.StateMachine.Language      as L
import           Control.StateMachine.Interpreter   as I
import qualified Control.StateMachine.Runtime       as R
import           Control.StateMachine.Domain        as D

eventAnalize, stateAnalize, stateMachineWorker
    :: IORef R.StateMaschineData -> StateMachine -> IO ()
eventAnalize stateMachineRef (StateMachine eventVar stateVar) = do
    event       <- getEvent =<< readChan eventVar
    machineData <- readIORef stateMachineRef
 
    mTransition <- R.takeTransition event machineData
    unless (isJust mTransition) $ do
        machineData ^. R.loger $ describe event
        R.applyStaticalDo machineData event
    whenJust mTransition $ \(R.Transition currentState newState) -> do
        machineData ^. R.loger $ showTransition currentState event newState
        void $ swapMVar stateVar newState
        modifyIORef stateMachineRef $ R.currentState .~ newState
        R.applyTransitionActions machineData currentState event newState

    stateAnalize stateMachineRef (StateMachine eventVar stateVar)

stateAnalize stateMachineRef stateMachine = do
    machineData <- readIORef stateMachineRef
    let currentState = machineData ^. R.currentState
    if R.isFinish machineData currentState
        then do
            machineData ^. R.loger $ "[finish state] " <> describe currentState
            R.applyExitDo machineData currentState
        else eventAnalize stateMachineRef stateMachine

showTransition :: MachineState -> MachineEvent -> MachineState -> Text
showTransition st1 ev st2 =
    "[transition] " <> describe st1 <> " -> " <> describe ev <> " -> " <> describe st2
stateMachineWorker = stateAnalize

runStateMachine :: Typeable a => Loger -> a -> StateMachineL () -> IO StateMachine
runStateMachine logerAction (toMachineState -> initState) machineDescriptione = do
    eventVar <- newChan
    stateVar <- newMVar initState
    textId   <- newTextId
    let stateMachine = StateMachine eventVar stateVar
    let loger txt = logerAction $ "[SM] " <> "[" <> textId  <> "] " <> txt
    void $ forkIO $ do
        stateMachineData <- makeStateMachineData loger initState stateMachine machineDescriptione
        stateMachineRef  <- newIORef stateMachineData
        
        loger $ "[init state] " <> describe initState
        R.applyEntryDo stateMachineData initState
        stateMachineWorker stateMachineRef stateMachine
    pure stateMachine

emit :: Typeable a => StateMachine -> a -> IO ()
emit (StateMachine eventVar _) = writeChan eventVar . D.FastEvent . D.toMachineEvent

takeState :: StateMachine -> IO MachineState
takeState (StateMachine _ stateVar) = readMVar stateVar

emitAndWait :: Typeable a => StateMachine -> a -> IO ()
emitAndWait (StateMachine eventVar _) event = do
    var <- newEmptyMVar
    writeChan eventVar $ D.WaitEvent (D.toMachineEvent event) var
    takeMVar var