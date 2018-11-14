{-# Language DeriveFunctor #-}
module Control.StateMachine.Language where

import           Universum
import           Control.Monad.Free
import           Control.StateMachine.Domain

data StateMachineF next where
    -- Construction of state mashine struct
    SetFinishState              :: MachineState -> (() -> next) -> StateMachineF next
    AddTransition               :: MachineState -> MachineEvent -> MachineState -> (() -> next) -> StateMachineF next
    AddConditionalTransition    :: MachineState -> EventType -> (MachineEvent -> Maybe MachineState) -> (() -> next) -> StateMachineF next
    -- Addition handlers to states and transitions of state mashine
    StaticalDo                  :: MachineState -> EventType -> (MachineEvent -> IO ()) -> (() -> next) -> StateMachineF next
    EntryDo                     :: MachineState -> IO () -> (() -> next) -> StateMachineF next
    EntryWithEventDo            :: MachineState -> EventType -> (MachineEvent -> IO ()) -> (() -> next) -> StateMachineF next
    TransitionDo                :: MachineState -> MachineState -> EventType -> (MachineEvent -> IO ()) -> (() -> next) -> StateMachineF next
    ExitWithEventDo             :: MachineState -> EventType -> (MachineEvent -> IO ()) -> (() -> next) -> StateMachineF next
    ExitDo                      :: MachineState -> IO () -> (() -> next) -> StateMachineF next
    deriving (Functor)

type StateMachineL = Free StateMachineF

setFinishState :: Typeable state => state -> StateMachineL ()
setFinishState state = liftF $ SetFinishState (toMachineState state) id

addTransition
    :: (Typeable state1, Typeable event, Typeable state2)
    => state1 -> event -> state2 -> StateMachineL ()
addTransition state1 event state2 =
    liftF $ AddTransition (toMachineState state1) (toMachineEvent event) (toMachineState state2) id

addConditionalTransition
    :: (Typeable state, Typeable event)
    => state -> (event -> Maybe MachineState) -> StateMachineL ()
addConditionalTransition state condition = liftF $ AddConditionalTransition
    (toMachineState state)
    (conditionToType condition)
    (condition . fromMachineEvent)
    id

entryDo :: Typeable state => state -> IO () -> StateMachineL ()
entryDo state action = liftF $ EntryDo (toMachineState state) action id

entryWithEventDo
    :: (Typeable state, Typeable action)
    => state -> (action -> IO ()) -> StateMachineL ()
entryWithEventDo state action = liftF $ EntryWithEventDo
    (toMachineState state) (actionToType action) (action . fromMachineEvent) id

staticalDo :: (Typeable state, Typeable event) => state -> (event -> IO ()) -> StateMachineL ()
staticalDo state action = liftF $ StaticalDo
    (toMachineState state) (actionToType action) (action . fromMachineEvent) id

transitionDo
    :: (Typeable state1, Typeable state2, Typeable event)
    => state1 -> state2 -> (event -> IO ()) -> StateMachineL ()
transitionDo state1 state2 action = liftF $ TransitionDo
    (toMachineState state1)
    (toMachineState state2)
    (actionToType action)
    (action . fromMachineEvent)
    id

exitWithEventDo :: (Typeable state, Typeable event) => state -> (event -> IO ()) -> StateMachineL ()
exitWithEventDo state action = liftF $ ExitWithEventDo
    (toMachineState state) (actionToType action) (action . fromMachineEvent) id

exitDo :: Typeable state => state -> IO () -> StateMachineL ()
exitDo state action = liftF $ ExitDo (toMachineState state) action id

just :: Typeable state => state -> Maybe MachineState
just = Just . toMachineState