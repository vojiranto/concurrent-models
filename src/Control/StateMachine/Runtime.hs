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
    , _initialState :: Maybe MachineState
    , _currentState :: Maybe MachineState
    , _finishStates :: S.Set MachineState
    , _entryDo      :: M.Map MachineState (IO ())
    , _exitDo       :: M.Map MachineState (IO ())
    }
makeLenses ''StateMaschineData

emptyData :: StateMaschineData
emptyData = StateMaschineData mempty Nothing Nothing mempty mempty mempty

addTransitionToMap :: MachineState -> MachineEvent -> MachineState -> TransitionMap -> TransitionMap
addTransitionToMap state1 event = M.insert (state1, event)
