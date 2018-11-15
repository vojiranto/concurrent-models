{-# Language ViewPatterns #-}
module Control.StateMachine
    ( StateMachine
    , StateMachineL
    , runStateMachine
    , initialiseAction
    , setFinishState
    , myLink
    , addTransition
    , addConditionalTransition
    , staticalDo
    , exitDo
    , exitWithEventDo
    , transitionDo
    , entryWithEventDo
    , entryDo
    , emit
    , waitEmit
    , just
    , nothing
    , logToConsole
    , logOff
    ) where

import           Universum hiding (ToText(..))
import           Control.Concurrent (forkIO)
import           Control.Concurrent.Chan
import           Control.StateMachine.Language      as L
import           Control.StateMachine.Interpreter   as I
import qualified Control.StateMachine.Runtime       as R
import           Control.StateMachine.Domain        as D

type Loger = Text -> IO ()

eventAnalize, stateAnalize, stateMachineWorker :: Loger -> IORef R.StateMaschineData -> StateMachine -> IO ()
eventAnalize loger stateMachineRef (StateMachine eventVar) = do
    event       <- getEvent =<< readChan eventVar
    machineData <- readIORef stateMachineRef
 
    mTransition <- R.takeTransition event machineData
    unless (isJust mTransition) $ do
        loger $ "[SM] [event] [" <> toText event <> "]"
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
            loger $ "[SM] [finish state] [" <> toText currentState <> "]" 
            R.apply currentState (machineData ^. R.exitDo)
        else eventAnalize loger stateMachineRef stateMachine

showTransition :: MachineState -> MachineEvent -> MachineState -> Text
showTransition st1 ev st2 =
    "[SM] [transition] [" <> toText st1 <> "] -> [" <> toText ev <> "] -> [" <> toText st2  <> "]"
stateMachineWorker = stateAnalize

runStateMachine :: Typeable a => Loger -> a -> StateMachineL () -> IO StateMachine
runStateMachine loger (toMachineState -> initState) machineDescriptione = do
    eventVar <- newChan
    let stateMachine = StateMachine eventVar
    void $ forkIO $ do
        stateMachineData <- makeStateMachineData loger initState stateMachine machineDescriptione
        stateMachineRef  <- newIORef stateMachineData
        
        loger $ "[SM] [init state] [" <> toText initState <> "]"
        R.apply initState (stateMachineData ^. R.entryDo)
        stateMachineWorker loger stateMachineRef stateMachine
    pure stateMachine

emit :: Typeable a => StateMachine -> a -> IO ()
emit (StateMachine eventVar) = writeChan eventVar . D.FastEvent . D.toMachineEvent

waitEmit :: Typeable a => StateMachine -> a -> IO ()
waitEmit (StateMachine eventVar) event = do
    var <- newEmptyMVar
    writeChan eventVar $ D.WaitEvent (D.toMachineEvent event) var
    takeMVar var

logToConsole :: Loger
logToConsole = putTextLn

logOff :: Loger
logOff _ = pure ()