{-# Language ViewPatterns #-}
module Control.StateMachine
    ( StateMachine
    , StateMachineL
    , makeStates
    , makeEvents
    , groupStates
    , runStateMachine
    , initialiseAction
    , setFinishState
    , getMyLink
    , addTransition
    , addConditionalTransition
    , L.staticalDo
    , L.exitDo
    , L.exitWithEventDo
    , L.entryWithEventDo
    , L.entryDo
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
import           Data.Flag
import           Data.Describe
import           Control.Loger
import           Control.Concurrent (forkIO)
import           Control.Concurrent.Chan

import           Control.StateMachine.TH
import           Control.StateMachine.Language                      as L
import           Control.StateMachine.Interpreter                   as I
import qualified Control.StateMachine.Runtime                       as R
import           Control.StateMachine.Runtime.StateMaschineHandlers as R
import           Control.StateMachine.Runtime.StateMaschineStruct   as R 
import           Control.StateMachine.Domain                        as D

eventAnalize, stateAnalize, stateMachineWorker
    :: IORef R.StateMaschineData -> StateMachine -> IO ()
eventAnalize stateMachineRef (StateMachine eventVar stateVar) = do
    (event, processed) <- getEvent =<< readChan eventVar
    machineData        <- readIORef stateMachineRef

    mTransition        <- R.takeTransition event machineData
    unless (isJust mTransition) $ do
        machineData ^. R.loger $ describe event
        let stList = R.takeGroups
                (machineData ^. R.stateMachineStruct)
                (machineData ^. R.currentState)
        forM_ stList $ \st ->
            R.applyStaticalDo (machineData ^. R.loger) (machineData ^. R.handlers) st event

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

runStateMachine :: Typeable a => Loger -> a -> StateMachineL () -> IO StateMachine
runStateMachine logerAction (toMachineState -> initState) machineDescriptione = do
    eventVar <- newChan
    stateVar <- newMVar initState
    textId   <- newTextId
    let stateMachine = StateMachine eventVar stateVar
    let loger txt = logerAction $ "[SM] " <> "[" <> textId  <> "] " <> txt
    void $ forkIO $ do
        mStateMachineData <- makeStateMachineData loger initState stateMachine machineDescriptione
        case mStateMachineData of
            Right stateMachineData -> do 
                stateMachineRef  <- newIORef stateMachineData
                
                loger $ "[init state] " <> describe initState
                let stList = R.takeGroups (stateMachineData ^. R.stateMachineStruct) initState
                forM_ stList (R.applyEntryDo loger (stateMachineData ^. R.handlers))
                stateMachineWorker stateMachineRef stateMachine
            Left err -> loger $ "[error] " <> show err
    pure stateMachine

emit :: Typeable a => StateMachine -> a -> IO ()
emit (StateMachine eventVar _) = writeChan eventVar . D.FastEvent . D.toMachineEvent

takeState :: StateMachine -> IO MachineState
takeState (StateMachine _ stateVar) = readMVar stateVar

emitAndWait :: Typeable a => StateMachine -> a -> IO ()
emitAndWait (StateMachine eventVar _) event = do
    processed <- newFlag
    writeChan eventVar $ D.WaitEvent (D.toMachineEvent event) processed
    wait processed
