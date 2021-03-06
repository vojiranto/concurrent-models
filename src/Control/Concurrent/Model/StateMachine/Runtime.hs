{-# Language TemplateHaskell #-}
{-# Language RankNTypes      #-}

module Control.Concurrent.Model.StateMachine.Runtime
    ( module X
    , StateMaschineData(..)
    , emptyData
    , takeTransition
    , applyTransitionActions
    , isFinish
    , handlers
    , stateMachineStruct
    , currentState
    , loger
    ) where

import           Control.Concurrent.Prelude
import qualified Data.Set as S

import           Control.Concurrent.Model.Core
import           Control.Concurrent.Model.StateMachine.Domain
import           Control.Concurrent.Model.StateMachine.Runtime.Struct as X
import           Control.Concurrent.Model.StateMachine.Runtime.Handlers as X

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