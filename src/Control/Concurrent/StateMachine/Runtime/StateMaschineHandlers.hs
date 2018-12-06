{-# Language TemplateHaskell #-}
{-# Language RankNTypes      #-}

module Control.Concurrent.StateMachine.Runtime.StateMaschineHandlers where

import           Universum
import           Control.Lens.At (at, Index, IxValue, At)
import           Control.Lens.Getter (Getting)
import           Control.Lens.TH
import qualified Data.Map as M

import           Control.Concurrent.StateMachine.Domain
import           Control.Concurrent.Core.Data
import           Control.Concurrent.Loger



data StateMaschineHandlers = StateMaschineHandlers
    { _staticalDo               :: M.Map (MachineState, EventType) (Event -> IO ())
    , _mathDo                   :: M.Map EventType (Event -> IO ())
    , _entryDo                  :: M.Map MachineState (IO ())
    , _entryWithEventDo         :: M.Map (MachineState, EventType) (Event -> IO ())
    , _exitWithEventDo          :: M.Map (MachineState, EventType) (Event -> IO ())
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
    :: Loger -> StateMaschineHandlers -> MachineState -> Event -> IO ()

applyExitWithEventDo  = applyEvent exitWithEventDo  "[onExit]"
applyEntryWithEventDo = applyEvent entryWithEventDo "[onEntry]"
applyStaticalDo       = applyEvent staticalDo       "[math]"

applyMath :: Loger -> StateMaschineHandlers -> Event -> IO ()
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
    :: (Index m ~ (MachineState, EventType), IxValue m ~ (Event -> IO ()), At m)
    => Getting (Maybe (Event -> IO ())) StateMaschineHandlers m
    -> Text -> Loger -> StateMaschineHandlers -> MachineState -> Event -> IO ()
applyEvent actionLens tag toLog machineData st event =
    whenJust (machineData ^. actionLens . at (st, (eventToType event))) $ \action -> do
        toLog $ tag <> " " <> describe st <> " "<> describe event
        action event

