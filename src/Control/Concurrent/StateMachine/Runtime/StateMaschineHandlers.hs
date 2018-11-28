{-# Language TemplateHaskell #-}
{-# Language RankNTypes      #-}

module Control.Concurrent.StateMachine.Runtime.StateMaschineHandlers where

import           Universum
import           Data.Describe
import           Control.Concurrent.Loger
import           Control.Lens.At (at, Index, IxValue, At)
import           Control.Lens.Getter (Getting)
import           Control.Lens.TH
import           Control.Concurrent.StateMachine.Domain
import qualified Data.Map as M

data StateMaschineHandlers = StateMaschineHandlers
    { _staticalDo               :: M.Map (MachineState, EventType) (MachineEvent -> IO ())
    , _mathDo                   :: M.Map EventType (MachineEvent -> IO ())
    , _entryDo                  :: M.Map MachineState (IO ())
    , _entryWithEventDo         :: M.Map (MachineState, EventType) (MachineEvent -> IO ())
    , _exitWithEventDo          :: M.Map (MachineState, EventType) (MachineEvent -> IO ())
    , _exitDo                   :: M.Map MachineState (IO ())
    }

makeLenses ''StateMaschineHandlers

emptyHandlers :: StateMaschineHandlers
emptyHandlers = StateMaschineHandlers mempty mempty mempty mempty mempty mempty

applyEntryDo, applyExitDo
    :: Loger -> StateMaschineHandlers -> MachineState -> IO ()

applyEntryDo = applyState entryDo "[onEntry]"
applyExitDo  = applyState exitDo  "[onExit]"

applyExitWithEventDo, applyEntryWithEventDo, applyStaticalDo
    :: Loger -> StateMaschineHandlers -> MachineState -> MachineEvent -> IO ()

applyExitWithEventDo  = applyEvent exitWithEventDo  "[onExit]"
applyEntryWithEventDo = applyEvent entryWithEventDo "[onEntry]"
applyStaticalDo       = applyEvent staticalDo       "[math]"

applyMath :: Loger -> StateMaschineHandlers -> MachineEvent -> IO ()
applyMath toLog machineData event =
    whenJust (machineData ^. mathDo . at (eventToType event)) $ \action -> do
        toLog $ "[math]" <> " "<> describe event
        action event

applyState
        :: (Index m ~ MachineState, IxValue m ~ IO (), At m)
        => Getting (Maybe (IO ())) StateMaschineHandlers m
        -> Text -> Loger -> StateMaschineHandlers -> MachineState -> IO ()
applyState actionLens tag toLog machineData st =
    whenJust (machineData ^. actionLens . at st) $ \action -> do
        toLog $ tag <> " " <> describe st
        action

applyEvent
    :: (Index m ~ (MachineState, EventType), IxValue m ~ (MachineEvent -> IO ()), At m)
    => Getting (Maybe (MachineEvent -> IO ())) StateMaschineHandlers m
    -> Text -> Loger -> StateMaschineHandlers -> MachineState -> MachineEvent -> IO ()
applyEvent actionLens tag toLog machineData st event =
    whenJust (machineData ^. actionLens . at (st, (eventToType event))) $ \action -> do
        toLog $ tag <> " " <> describe st <> " "<> describe event
        action event

