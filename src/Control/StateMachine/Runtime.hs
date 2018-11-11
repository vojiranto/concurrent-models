{-# Language TemplateHaskell #-}

module Control.StateMachine.Runtime where

import           Universum
import           Control.Lens.TH
import           Control.StateMachine.Domain
import qualified Data.Map as M
import qualified Data.Set as S

type TransitionMap = M.Map (MachineState, MachineEvent) MachineState

data StateMaschineData = StateMaschineData
    { _transitions  :: TransitionMap 
    , _currentState :: MachineState
    , _finishStates :: S.Set MachineState
    , _entryDo      :: M.Map MachineState (IO ())
    , _exitDo       :: M.Map MachineState (IO ())
    }
makeLenses ''StateMaschineData

emptyData :: MachineState -> StateMaschineData
emptyData initState = StateMaschineData mempty initState mempty mempty mempty

addTransitionToMap :: MachineState -> MachineEvent -> MachineState -> TransitionMap -> TransitionMap
addTransitionToMap state1 event = M.insert (state1, event)

apply :: MachineState -> M.Map MachineState (IO ()) -> IO ()
apply state ioHandlers = whenJust (state `M.lookup` ioHandlers) id

takeTransition :: MachineState -> MachineEvent -> StateMaschineData -> Maybe MachineState
takeTransition state1 event maschineData =
    (state1, event) `M.lookup` (maschineData ^. transitions)