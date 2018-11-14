{-# Language TemplateHaskell #-}

module Control.StateMachine.Runtime where

import           Universum
import           Control.Lens.TH
import           Control.StateMachine.Domain
import qualified Data.Map as M
import qualified Data.Set as S

type TransitionMap     = M.Map (MachineState, EventType) MachineState
type TransitionActions = M.Map (MachineState, EventType, MachineState) (MachineEvent -> IO ())
data Transition        = Transition MachineState MachineState

data StateMaschineData = StateMaschineData
    { _transitions      :: TransitionMap
    , _currentState     :: MachineState
    , _finishStates     :: S.Set MachineState
    , _entryDo          :: M.Map MachineState (IO ())
    , _entryWithEventDo :: M.Map (MachineState, EventType) (MachineEvent -> IO ())
    , _transitionDo     :: TransitionActions
    , _exitWithEventDo  :: M.Map (MachineState, EventType) (MachineEvent -> IO ())
    , _exitDo           :: M.Map MachineState (IO ())
    }
makeLenses ''StateMaschineData

emptyData :: MachineState -> StateMaschineData
emptyData initState = StateMaschineData mempty initState mempty mempty mempty mempty mempty mempty

addTransitionToMap :: MachineState -> MachineEvent -> MachineState -> TransitionMap -> TransitionMap
addTransitionToMap state1 event = M.insert (state1, eventToType event)

apply :: MachineState -> M.Map MachineState (IO ()) -> IO ()
apply state ioHandlers = whenJust (state `M.lookup` ioHandlers) id

takeTransition :: MachineEvent -> StateMaschineData -> Maybe Transition
takeTransition event maschineData =
    case (currentState', eventToType event) `M.lookup` transitionMap of
        Just newState -> Just $ Transition currentState' newState
        Nothing       -> Nothing
    where
        currentState' :: MachineState
        currentState'  = maschineData ^. currentState
        
        transitionMap :: TransitionMap
        transitionMap = maschineData ^. transitions

applyExitAction, applyEntryAction :: StateMaschineData -> MachineState -> IO ()
applyExitAction  machineData state = apply state (machineData ^. exitDo)
applyEntryAction machineData state = apply state (machineData ^. entryDo)

applyTransition :: StateMaschineData -> MachineState -> MachineEvent -> MachineState -> IO ()
applyTransition machineData st1 event st2 =
    whenJust ((st1, eventToType event, st2) `M.lookup` (machineData ^. transitionDo)) $
        \action -> action event

applyEntryWithEventDo :: StateMaschineData -> MachineState -> MachineEvent -> IO ()
applyEntryWithEventDo machineData st1 event =
    whenJust ((st1, eventToType event) `M.lookup` (machineData ^. entryWithEventDo)) $
        \action -> action event

applyExitWithEventDo :: StateMaschineData -> MachineState -> MachineEvent -> IO ()
applyExitWithEventDo machineData st1 event =
    whenJust ((st1, eventToType event) `M.lookup` (machineData ^. exitWithEventDo)) $
        \action -> action event


isFinish :: StateMaschineData -> MachineState -> Bool
isFinish machineData state = S.member state (machineData ^. finishStates)