{-# Language TemplateHaskell #-}
{-# Language RankNTypes      #-}

module Control.Concurrent.StateMachine.Runtime.StateMaschineHandlers where

import           Universum
import           Data.Describe
import           Control.Concurrent.Loger
import           Control.Lens.At (at, Index, IxValue, At)
import           Control.Lens.TH
import           Control.Concurrent.StateMachine.Domain
import qualified Data.Map as M

data StateMaschineHandlers = StateMaschineHandlers
    { _staticalDo               :: M.Map (MachineState, EventType) (MachineEvent -> IO ())
    , _entryDo                  :: M.Map MachineState (IO ())
    , _entryWithEventDo         :: M.Map (MachineState, EventType) (MachineEvent -> IO ())
    , _exitWithEventDo          :: M.Map (MachineState, EventType) (MachineEvent -> IO ())
    , _exitDo                   :: M.Map MachineState (IO ())
    }

makeLenses ''StateMaschineHandlers

emptyHandlers :: StateMaschineHandlers
emptyHandlers = StateMaschineHandlers mempty mempty mempty mempty mempty

applyEntryDo :: Loger -> StateMaschineHandlers -> MachineState -> IO ()
applyEntryDo toLog machineData st =
    whenJust (machineData ^. entryDo . at st) $ \action -> do
        toLog  $ "[entry do] " <> describe st
        action

applyExitDo :: Loger -> StateMaschineHandlers -> MachineState -> IO ()
applyExitDo toLog machineData st =
    whenJust (machineData ^. exitDo . at st) $ \action -> do
        toLog $ "[exit do] " <> describe st
        action

applyExitWithEventDo :: Loger -> StateMaschineHandlers -> MachineState -> MachineEvent -> IO ()
applyExitWithEventDo toLog machineData st event =
    whenJust (machineData ^.  exitWithEventDo . at2 st (eventToType event)) $ \action -> do
        toLog $ "[exit with event do] " <> describe st <> " " <> describe event
        action event

applyEntryWithEventDo :: Loger -> StateMaschineHandlers -> MachineState -> MachineEvent -> IO ()
applyEntryWithEventDo toLog machineData st event =
    whenJust (machineData ^. entryWithEventDo . at2 st (eventToType event)) $ \action -> do
        toLog $ "[entry with event do] " <> describe st <> " " <> describe event
        action event

applyStaticalDo :: Loger -> StateMaschineHandlers -> MachineState -> MachineEvent -> IO ()
applyStaticalDo toLog machineData st event =
    whenJust (machineData ^. staticalDo . at2 st (eventToType event)) $ \action -> do
        toLog $ "[statical do] " <> describe st <> describe event
        action event

at2 :: (Index m ~ (a, b), At m) => a -> b -> Lens' m (Maybe (IxValue m))
at2 x y = at (x, y)
