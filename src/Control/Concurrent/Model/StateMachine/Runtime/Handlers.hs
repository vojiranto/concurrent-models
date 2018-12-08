{-# Language TemplateHaskell #-}
{-# Language RankNTypes      #-}

module Control.Concurrent.Model.StateMachine.Runtime.Handlers where

import           Control.Concurrent.Prelude
import qualified Data.Map as M

import           Control.Concurrent.Model.StateMachine.Domain
import           Control.Concurrent.Model.Core

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
applyMath toLog' machineData event =
    whenJust (machineData ^. mathDo . at (eventToType event)) $ \action -> do
        toLog' Trace $ "[math]" <> " " <> describe event
        action event

applyState
        :: (Index m ~ MachineState, IxValue m ~ IO (), At m)
        => Getting (Maybe (IO ())) StateMaschineHandlers m
        -> Text -> Loger -> StateMaschineHandlers -> MachineState -> IO ()
applyState actionLens tag toLog' machineData st =
    whenJust (machineData ^. actionLens . at st) $ \action -> do
        toLog' Trace $ tag <> " " <> describe st
        action

applyEvent
    :: (Index m ~ (MachineState, EventType), IxValue m ~ (Event -> IO ()), At m)
    => Getting (Maybe (Event -> IO ())) StateMaschineHandlers m
    -> Text -> Loger -> StateMaschineHandlers -> MachineState -> Event -> IO ()
applyEvent actionLens tag toLog' machineData st event =
    whenJust (machineData ^. actionLens . at (st, eventToType event)) $ \action -> do
        toLog' Trace $ tag <> " " <> describe st <> " "<> describe event
        action event

