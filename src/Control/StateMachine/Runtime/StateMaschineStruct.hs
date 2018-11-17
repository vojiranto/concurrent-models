{-# Language TemplateHaskell #-}
{-# Language RankNTypes      #-}

module Control.StateMachine.Runtime.StateMaschineStruct where

import           Universum
import           Control.Loger
import           Control.Lens.TH
import           Control.StateMachine.Domain
import qualified Data.Map as M
import qualified Data.Set as S

type TransitionMap          = M.Map (MachineState, EventType) MachineState
type ConditionalTransitions = M.Map (MachineState, EventType) (MachineEvent -> IO (Maybe MachineState))

data StateMaschineStruct = StateMaschineStruct
    { _transitions              :: TransitionMap
    , _conditionalTransitions   :: ConditionalTransitions
    , _finishStates             :: S.Set MachineState
    , _groups                   :: S.Set MachineState
    , _groupStruct              :: M.Map MachineState MachineState
    }

makeLenses ''StateMaschineStruct

emptyStruct :: StateMaschineStruct
emptyStruct = StateMaschineStruct mempty mempty mempty mempty mempty

takeTransitionFromStruct :: Loger -> MachineState -> MachineEvent -> StateMaschineStruct -> IO (Maybe Transition)
takeTransitionFromStruct _ currentState' event maschineData =
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
        
        transitionMap :: TransitionMap
        transitionMap = maschineData ^. transitions

        conditionals :: ConditionalTransitions
        conditionals = maschineData ^. conditionalTransitions


checkStruct :: StateMaschineStruct -> Bool
checkStruct struct = allTransitionIsValid && structIsValid
    where
        structIsValid = structIsEmpty || structIsTree
        structIsEmpty = null (struct ^. groups)

        -- it is true if no parent for any group
        structIsTree =
            any (\g -> M.notMember g (struct ^. groupStruct)) (struct ^. groups)

        -- not exist transition to group
        allTransitionIsValid =
            all (\s -> S.notMember s (struct ^. groups)) (M.elems (struct ^. transitions))