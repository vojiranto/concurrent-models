{-# Language TemplateHaskell #-}

module Control.StateMachine.Runtime where

import           Universum
import           Control.Lens.TH
import           Control.StateMachine.Domain
import qualified Data.Map as M
import qualified Data.Set as S

type TransitionMap          = M.Map (MachineState, EventType) MachineState
type ConditionalTransitions = M.Map (MachineState, EventType) (MachineEvent -> IO (Maybe MachineState))
type TransitionActions      = M.Map (MachineState, EventType, MachineState) (MachineEvent -> IO ())
data Transition             = Transition MachineState MachineState

data StateMaschineData = StateMaschineData
    { _transitions              :: TransitionMap
    , _conditionalTransitions   :: ConditionalTransitions
    , _currentState             :: MachineState
    , _finishStates             :: S.Set MachineState
    , _staticalDo               :: M.Map (MachineState, EventType) (MachineEvent -> IO ())
    , _entryDo                  :: M.Map MachineState (IO ())
    , _entryWithEventDo         :: M.Map (MachineState, EventType) (MachineEvent -> IO ())
    , _transitionDo             :: TransitionActions
    , _exitWithEventDo          :: M.Map (MachineState, EventType) (MachineEvent -> IO ())
    , _exitDo                   :: M.Map MachineState (IO ())
    }
makeLenses ''StateMaschineData

emptyData :: MachineState -> StateMaschineData
emptyData initState =
    StateMaschineData mempty mempty initState mempty mempty mempty mempty mempty mempty mempty

takeTransition :: MachineEvent -> StateMaschineData -> IO (Maybe Transition)
takeTransition event maschineData =
    case lookupByEvent transitionMap of
        Just newState -> pure . Just $ Transition currentState' newState
        Nothing       -> case lookupByEvent conditionals of
            Just condition -> do
                mNewState <- condition event
                case mNewState of
                    Just newState -> pure . Just $ Transition currentState' newState
                    Nothing       -> pure Nothing
            Nothing        -> pure Nothing
    where
        lookupByEvent :: M.Map (MachineState, EventType) a -> Maybe a 
        lookupByEvent = M.lookup (currentState', eventToType event)

        currentState' :: MachineState
        currentState'  = maschineData ^. currentState
        
        transitionMap :: TransitionMap
        transitionMap = maschineData ^. transitions

        conditionals :: ConditionalTransitions
        conditionals = maschineData ^. conditionalTransitions

apply :: MachineState -> M.Map MachineState (IO ()) -> IO ()
apply state actionMap = whenJust (state `M.lookup` actionMap) id

applyEvent :: MachineState -> MachineEvent -> M.Map (MachineState, EventType) (MachineEvent -> IO ()) -> IO ()
applyEvent state event actionMap = whenJust ((state, eventToType event) `M.lookup` actionMap) ($ event)

applyTransitionActions :: StateMaschineData -> MachineState -> MachineEvent -> MachineState -> IO ()
applyTransitionActions machineData state1 event state2 = do
    let eventType = eventToType event
    apply state1 (machineData ^. exitDo)
    applyEvent state1 event (machineData ^. exitWithEventDo)
    whenJust ((state1, eventType, state2) `M.lookup` (machineData ^. transitionDo)) ($ event)    
    applyEvent state2 event (machineData ^. entryWithEventDo)
    apply state2 (machineData ^. entryDo)

isFinish :: StateMaschineData -> MachineState -> Bool
isFinish machineData state = S.member state (machineData ^. finishStates)