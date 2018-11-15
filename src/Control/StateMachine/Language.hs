{-# Language TemplateHaskell       #-}
module Control.StateMachine.Language
    ( StateMachineF(..)
    , StateMachineL
    , initialiseAction
    , setFinishState
    , addTransition
    , addConditionalTransition
    , staticalDo
    , entryDo
    , entryWithEventDo
    , transitionDo
    , exitWithEventDo
    , exitDo
    , just
    , nothing
    ) where

import           Universum
import           Language.Haskell.TH.MakeFunctor
import           Control.Monad.Free
import           Control.StateMachine.Domain

data StateMachineF next where
    InitialiseAction            :: IO a -> (a -> next) -> StateMachineF next
    -- Construction of state mashine struct
    SetFinishState              :: MachineState -> (() -> next) -> StateMachineF next
    AddTransition               :: MachineState -> MachineEvent -> MachineState -> (() -> next) -> StateMachineF next
    AddConditionalTransition    :: MachineState -> EventType -> (MachineEvent -> IO (Maybe MachineState)) -> (() -> next) -> StateMachineF next
    -- Addition handlers to states and transitions of state mashine
    StaticalDo                  :: MachineState -> EventType -> (MachineEvent -> IO ()) -> (() -> next) -> StateMachineF next
    EntryDo                     :: MachineState -> IO () -> (() -> next) -> StateMachineF next
    EntryWithEventDo            :: MachineState -> EventType -> (MachineEvent -> IO ()) -> (() -> next) -> StateMachineF next
    TransitionDo                :: MachineState -> MachineState -> EventType -> (MachineEvent -> IO ()) -> (() -> next) -> StateMachineF next
    ExitWithEventDo             :: MachineState -> EventType -> (MachineEvent -> IO ()) -> (() -> next) -> StateMachineF next
    ExitDo                      :: MachineState -> IO () -> (() -> next) -> StateMachineF next
makeFunctorInstance ''StateMachineF

type StateMachineL = Free StateMachineF

initialiseAction :: IO a -> StateMachineL a
initialiseAction action = liftF $ InitialiseAction action id

setFinishState :: Typeable state => state -> StateMachineL ()
setFinishState finishState = liftF $
    SetFinishState (toMachineState finishState) id

addTransition
    :: (Typeable state1, Typeable event, Typeable state2)
    => state1 -> event -> state2 -> StateMachineL ()
addTransition state1 event state2 =
    liftF $ AddTransition (toMachineState state1) (toMachineEvent event) (toMachineState state2) id

addConditionalTransition
    :: (Typeable state, Typeable event)
    => state -> (event -> IO (Maybe MachineState)) -> StateMachineL ()
addConditionalTransition currentState condition = liftF $ AddConditionalTransition
    (toMachineState currentState)
    (conditionToType condition)
    (condition . fromMachineEvent)
    id

entryDo :: Typeable state => state -> IO () -> StateMachineL ()
entryDo newState action = liftF $ EntryDo (toMachineState newState) action id

entryWithEventDo
    :: (Typeable state, Typeable action)
    => state -> (action -> IO ()) -> StateMachineL ()
entryWithEventDo newState action = liftF $ EntryWithEventDo
    (toMachineState newState) (actionToType action) (action . fromMachineEvent) id

staticalDo :: (Typeable state, Typeable event) => state -> (event -> IO ()) -> StateMachineL ()
staticalDo currentState action = liftF $ StaticalDo
    (toMachineState currentState) (actionToType action) (action . fromMachineEvent) id

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
exitWithEventDo oldState action = liftF $ ExitWithEventDo
    (toMachineState oldState) (actionToType action) (action . fromMachineEvent) id

exitDo :: Typeable state => state -> IO () -> StateMachineL ()
exitDo oldState action = liftF $ ExitDo (toMachineState oldState) action id

just :: Typeable state => state -> IO (Maybe MachineState)
just = pure . Just . toMachineState

nothing :: IO (Maybe MachineState)
nothing = pure Nothing
