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
    { _transitions              :: TransitionMap
    , _conditionalTransitions   :: M.Map (MachineState, EventType) (MachineEvent -> Maybe MachineState)
    , _currentState             :: MachineState
    , _finishStates             :: S.Set MachineState
    , _entryDo                  :: M.Map MachineState (IO ())
    , _entryWithEventDo         :: M.Map (MachineState, EventType) (MachineEvent -> IO ())
    , _transitionDo             :: TransitionActions
    , _exitWithEventDo          :: M.Map (MachineState, EventType) (MachineEvent -> IO ())
    , _exitDo                   :: M.Map MachineState (IO ())
    }
makeLenses ''StateMaschineData

emptyData :: MachineState -> StateMaschineData
emptyData initState = StateMaschineData mempty mempty initState mempty mempty mempty mempty mempty mempty

apply :: MachineState -> M.Map MachineState (IO ()) -> IO ()
apply state ioHandlers = whenJust (state `M.lookup` ioHandlers) id

takeTransition :: MachineEvent -> StateMaschineData -> Maybe Transition
takeTransition event maschineData =
    case lookupByEvent transitionMap of
        Just newState -> Just $ Transition currentState' newState
        Nothing       -> case lookupByEvent conditionals of
            Just condition -> case condition event of
                Just newState -> Just $ Transition currentState' newState
                Nothing       -> Nothing
            Nothing        -> Nothing
    where
        lookupByEvent :: M.Map (MachineState, EventType) a -> Maybe a 
        lookupByEvent = M.lookup (currentState', eventToType event)

        currentState' :: MachineState
        currentState'  = maschineData ^. currentState
        
        transitionMap :: TransitionMap
        transitionMap = maschineData ^. transitions

        conditionals :: M.Map (MachineState, EventType) (MachineEvent -> Maybe MachineState)
        conditionals = maschineData ^. conditionalTransitions

applyTransitionActions :: StateMaschineData -> MachineState -> MachineEvent -> MachineState -> IO ()
applyTransitionActions machineData state1 event state2 = do
    let eventType = eventToType event
    apply state1 (machineData ^. exitDo)
    whenJust ((state1, eventType) `M.lookup` (machineData ^. exitWithEventDo))      ($ event)
    whenJust ((state1, eventType, state2) `M.lookup` (machineData ^. transitionDo)) ($ event)    
    whenJust ((state2, eventType) `M.lookup` (machineData ^. entryWithEventDo))     ($ event)
    apply state2 (machineData ^. entryDo)

isFinish :: StateMaschineData -> MachineState -> Bool
isFinish machineData state = S.member state (machineData ^. finishStates)