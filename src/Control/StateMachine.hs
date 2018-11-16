{-# Language ViewPatterns #-}
module Control.StateMachine
    ( StateMachine
    , StateMachineL
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
    ) where

import           Universum
import           Data.TextId
import           Data.Describe
import           Control.Loger
import           Control.Concurrent (forkIO)
import           Control.Concurrent.Chan
import           Control.StateMachine.Language      as L
import           Control.StateMachine.Interpreter   as I
import qualified Control.StateMachine.Runtime       as R
import           Control.StateMachine.Domain        as D

eventAnalize, stateAnalize, stateMachineWorker
    :: Loger -> IORef R.StateMaschineData -> StateMachine -> IO ()
eventAnalize loger stateMachineRef (StateMachine eventVar) = do
    event       <- getEvent =<< readChan eventVar
    machineData <- readIORef stateMachineRef
 
    mTransition <- R.takeTransition event machineData
    unless (isJust mTransition) $ do
        loger $ describe event
        R.applyEvent (machineData ^. R.currentState) event (machineData ^. R.staticalDo)
    whenJust mTransition $ \(R.Transition currentState newState) -> do
        loger $ showTransition currentState event newState
        R.applyTransitionActions machineData currentState event newState
        modifyIORef stateMachineRef $ R.currentState .~ newState
    stateAnalize loger stateMachineRef (StateMachine eventVar)

stateAnalize loger stateMachineRef stateMachine = do
    machineData <- readIORef stateMachineRef
    let currentState = machineData ^. R.currentState
    if R.isFinish machineData currentState
        then do
            loger $ "[finish state] " <> describe currentState
            R.apply currentState (machineData ^. R.exitDo)
        else eventAnalize loger stateMachineRef stateMachine

showTransition :: MachineState -> MachineEvent -> MachineState -> Text
showTransition st1 ev st2 =
    "[transition] " <> describe st1 <> " -> " <> describe ev <> " -> " <> describe st2
stateMachineWorker = stateAnalize

runStateMachine :: Typeable a => Loger -> a -> StateMachineL () -> IO StateMachine
runStateMachine logerAction (toMachineState -> initState) machineDescriptione = do
    eventVar <- newChan
    textId   <- newTextId
    let stateMachine = StateMachine eventVar
    let loger txt = logerAction $ "[SM] " <> "[" <> textId  <> "] " <> txt
    void $ forkIO $ do
        stateMachineData <- makeStateMachineData loger initState stateMachine machineDescriptione
        stateMachineRef  <- newIORef stateMachineData
        
        loger $ "[init state] " <> describe initState
        R.apply initState (stateMachineData ^. R.entryDo)
        stateMachineWorker loger stateMachineRef stateMachine
    pure stateMachine

emit :: Typeable a => StateMachine -> a -> IO ()
emit (StateMachine eventVar) = writeChan eventVar . D.FastEvent . D.toMachineEvent

emitAndWait :: Typeable a => StateMachine -> a -> IO ()
emitAndWait (StateMachine eventVar) event = do
    var <- newEmptyMVar
    writeChan eventVar $ D.WaitEvent (D.toMachineEvent event) var
    takeMVar var