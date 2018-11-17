{-# Language TemplateHaskell #-}
{-# Language RankNTypes      #-}

module Control.StateMachine.Runtime.StateMaschineStruct where

import           Universum
import           Control.Lens.At (at)
import           Data.Describe
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
takeTransitionFromStruct loger currentState =
    takeTransitionWithGroup loger currentState currentState

takeTransitionWithGroup :: Loger -> MachineState -> MachineState -> MachineEvent -> StateMaschineStruct -> IO (Maybe Transition)
takeTransitionWithGroup loger currentGroup currentState event maschineData = do
    mTrasition <- makeTransition currentState (lookupByEvent (maschineData ^. transitions)) $
        case lookupByEvent conditionals of
            Just condition -> do
                mNewState <- condition event
                makeTransition currentState mNewState (pure Nothing)
            Nothing        -> pure Nothing
    ok <- checkTransition loger maschineData mTrasition
    let superGroup = maschineData ^. groupStruct . at currentGroup
    if ok then pure mTrasition
    else case superGroup of
        Just gr -> takeTransitionWithGroup loger gr currentState event maschineData
        Nothing -> pure Nothing

    where
        lookupByEvent :: M.Map (MachineState, EventType) a -> Maybe a 
        lookupByEvent = M.lookup (currentGroup, eventToType event)
    
        conditionals :: ConditionalTransitions
        conditionals = maschineData ^. conditionalTransitions

checkTransition :: Loger -> StateMaschineStruct -> Maybe Transition -> IO Bool
checkTransition loger maschineData (Just (Transition st1 st2)) = do
    let err = S.member st2 (maschineData ^. groups)
    when err $ loger $ "[error trasition] " <> describe st1 <> " -> "<> describe st2 
    pure $ not err

checkTransition _ _ _ = pure False

makeTransition :: MachineState -> Maybe MachineState -> IO (Maybe Transition) -> IO (Maybe Transition)
makeTransition currentState mState def = 
    case mState of
        Just newState -> pure . Just $ Transition currentState newState
        Nothing       -> def

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