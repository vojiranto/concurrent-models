{-# Language DeriveFunctor #-}
module Control.StateMachine.Language where

import           Universum
import           Control.Monad.Free
import           Control.StateMachine.Domain

data StateMachineF next where
    -- Construction of state mashine struct
    SetFinishState      :: MachineState -> (() -> next) -> StateMachineF next
    AddTransition       :: MachineState -> MachineEvent -> MachineState -> (() -> next) -> StateMachineF next
    -- Addition handlers to states and transitions of state mashine
    EntryDo             :: MachineState -> IO () -> (() -> next) -> StateMachineF next
    EntryWithEventDo    :: MachineState -> EventType -> (MachineEvent -> IO ()) -> (() -> next) -> StateMachineF next
    TransitionDo        :: MachineState -> MachineState -> EventType -> (MachineEvent -> IO ()) -> (() -> next) -> StateMachineF next
    ExitWithEventDo     :: MachineState -> EventType -> (MachineEvent -> IO ()) -> (() -> next) -> StateMachineF next
    ExitDo              :: MachineState -> IO () -> (() -> next) -> StateMachineF next
    deriving (Functor)

type StateMachineL = Free StateMachineF

setFinishState :: Typeable a => a -> StateMachineL ()
setFinishState state = liftF $ SetFinishState (toMachineState state) id

addTransition :: (Typeable a, Typeable b, Typeable c) => a -> b -> c -> StateMachineL ()
addTransition state1 event state2 =
    liftF $ AddTransition (toMachineState state1) (toMachineEvent event) (toMachineState state2) id

entryDo :: Typeable a => a -> IO () -> StateMachineL ()
entryDo state io = liftF $ EntryDo (toMachineState state) io id

entryWithEventDo :: (Typeable a, Typeable b) => a -> (b -> IO ()) -> StateMachineL ()
entryWithEventDo st action = liftF $ EntryWithEventDo
    (toMachineState st) (actionToType action) (action . fromMachineEvent) id

transitionDo :: (Typeable a, Typeable b, Typeable c) => a -> b -> (c -> IO ()) -> StateMachineL ()
transitionDo st1 st2 action =
    liftF $ TransitionDo (toMachineState st1) (toMachineState st2) (actionToType action) (action . fromMachineEvent) id

exitWithEventDo :: (Typeable a, Typeable b) => a -> (b -> IO ()) -> StateMachineL ()
exitWithEventDo st action = liftF $ ExitWithEventDo
    (toMachineState st) (actionToType action) (action . fromMachineEvent) id

exitDo :: Typeable a => a -> IO () -> StateMachineL ()
exitDo state io = liftF $ ExitDo (toMachineState state) io id