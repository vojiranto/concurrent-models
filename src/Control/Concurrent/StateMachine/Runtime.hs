{-# Language TemplateHaskell #-}
{-# Language RankNTypes      #-}

module Control.Concurrent.StateMachine.Runtime where

import           Universum
import           Data.Event
import           Control.Concurrent.Loger
import           Control.Lens.TH
import           Control.Concurrent.StateMachine.Domain
import           Control.Concurrent.StateMachine.Runtime.StateMaschineStruct
import           Control.Concurrent.StateMachine.Runtime.StateMaschineHandlers
import qualified Data.Set as S

data StateMaschineData = StateMaschineData
    { _stateMachineStruct       :: StateMaschineStruct
    , _handlers                 :: StateMaschineHandlers
    , _currentState             :: MachineState
    , _loger                    :: Loger
    }
makeLenses ''StateMaschineData

emptyData :: Loger -> MachineState -> StateMaschineData
emptyData loger' initState =
    StateMaschineData emptyStruct emptyHandlers initState loger'

takeTransition :: Event -> StateMaschineData -> IO (Maybe Transition)
takeTransition event maschineData = takeTransitionFromStruct
    (maschineData ^. loger)
    (maschineData ^. currentState)
    event
    (maschineData ^. stateMachineStruct)

applyTransitionActions :: StateMaschineData -> MachineState -> Event -> MachineState -> IO ()
applyTransitionActions machineData oldState event newState = do
    let oldGroups = takeGroups (machineData ^. stateMachineStruct) oldState
    let newGroups = takeGroups (machineData ^. stateMachineStruct) newState
    let (exitGroups, entryGroups) = compareGroups oldGroups newGroups
    
    forM_ exitGroups $ \oldGroup -> do
        applyExitDo             (machineData ^. loger) (machineData ^. handlers) oldGroup
        applyExitWithEventDo    (machineData ^. loger) (machineData ^. handlers) oldGroup event
    forM_ entryGroups $ \newGroup -> do
        applyEntryWithEventDo   (machineData ^. loger) (machineData ^. handlers) newGroup event
        applyEntryDo            (machineData ^. loger) (machineData ^. handlers) newGroup

isFinish :: StateMaschineData -> Bool
isFinish machineData =
    S.member (machineData ^. currentState) (machineData ^. stateMachineStruct . finishStates)