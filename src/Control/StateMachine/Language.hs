{-# Language TemplateHaskell       #-}
module Control.StateMachine.Language
    ( StateMachineF(..)
    , StateMachineL
    , StateMachine(..)
    , this
    , groupStates
    , addFinalState
    , addTransition
    , addConditionalTransition
    , staticalDo
    , entryDo
    , entryWithEventDo
    , exitWithEventDo
    , exitDo
    , just
    , nothing
    ) where

import           Universum
import           Language.Haskell.TH.MakeFunctor
import           Control.Concurrent.Chan
import           Control.Monad.Free
import           Data.This
import           Control.StateMachine.Domain

data StateMachine = StateMachine (Chan Event) (MVar MachineState)

data StateMachineF next where
    LiftIO                      :: IO a -> (a -> next) -> StateMachineF next
    This                        :: (StateMachine -> next) -> StateMachineF next
    -- Construction of state mashine struct
    GroupStates                 :: MachineState -> [MachineState] -> (() -> next) -> StateMachineF next
    AddFinalState              :: MachineState -> (() -> next) -> StateMachineF next
    AddTransition               :: MachineState -> MachineEvent -> MachineState -> (() -> next) -> StateMachineF next
    AddConditionalTransition    :: MachineState -> EventType -> (MachineEvent -> IO (Maybe MachineState)) -> (() -> next) -> StateMachineF next
    -- Addition handlers to states and transitions of state mashine
    StaticalDo                  :: MachineState -> EventType -> (MachineEvent -> IO ()) -> (() -> next) -> StateMachineF next
    EntryDo                     :: MachineState -> IO () -> (() -> next) -> StateMachineF next
    EntryWithEventDo            :: MachineState -> EventType -> (MachineEvent -> IO ()) -> (() -> next) -> StateMachineF next
    ExitWithEventDo             :: MachineState -> EventType -> (MachineEvent -> IO ()) -> (() -> next) -> StateMachineF next
    ExitDo                      :: MachineState -> IO () -> (() -> next) -> StateMachineF next
makeFunctorInstance ''StateMachineF

type StateMachineL = Free StateMachineF

instance MonadIO StateMachineL where
    liftIO action = liftF $ LiftIO action id

instance This StateMachineL StateMachine where
    -- | Return link of current FSM.
    this = liftF $ This id

-- | Add new finsh state to FSM. 
addFinalState :: Typeable state => state -> StateMachineL ()
addFinalState finishState = liftF $
    AddFinalState (toMachineState finishState) id

-- | Group states, to be able to assign common handlers and transitions.
--   You can combine groups into groups of higher order, if necessary.
groupStates :: Typeable group => group -> [MachineState] -> StateMachineL ()
groupStates groupName states = liftF $ GroupStates (toMachineState groupName) states id

-- | Adds a transition between any two states.
--   Attention!! Target state should not be a group.
addTransition
    :: (Typeable state1, Typeable event, Typeable state2)
    => state1 -> event -> state2 -> StateMachineL ()
addTransition state1 event state2 =
    liftF $ AddTransition (toMachineState state1) (toMachineEvent event) (toMachineState state2) id

-- | Add a conditional transition, if the function returns nothing, the transition will not occur.
addConditionalTransition
    :: (Typeable state, Typeable event)
    => state -> (event -> IO (Maybe MachineState)) -> StateMachineL ()
addConditionalTransition currentState condition = liftF $ AddConditionalTransition
    (toMachineState currentState)
    (conditionToType condition)
    (condition . fromMachineEvent)
    id

-- | Execute the handler if the machine enters the state.
entryDo :: Typeable state => state -> IO () -> StateMachineL ()
entryDo newState action = liftF $ EntryDo (toMachineState newState) action id

-- | Execute the handler if the machine enters the state by event.
entryWithEventDo
    :: (Typeable state, Typeable action)
    => state -> (action -> IO ()) -> StateMachineL ()
entryWithEventDo newState action = liftF $ EntryWithEventDo
    (toMachineState newState) (actionToType action) (action . fromMachineEvent) id

-- | If event does not cause a transition, call handler with the event.
staticalDo :: (Typeable state, Typeable event) => state -> (event -> IO ()) -> StateMachineL ()
staticalDo currentState action = liftF $ StaticalDo
    (toMachineState currentState) (actionToType action) (action . fromMachineEvent) id

-- | Execute the handler if the machine exit the state state by event.
exitWithEventDo :: (Typeable state, Typeable event) => state -> (event -> IO ()) -> StateMachineL ()
exitWithEventDo oldState action = liftF $ ExitWithEventDo
    (toMachineState oldState) (actionToType action) (action . fromMachineEvent) id

-- | Execute the handler if the machine exit the state state.
exitDo :: Typeable state => state -> IO () -> StateMachineL ()
exitDo oldState action = liftF $ ExitDo (toMachineState oldState) action id

-- | A wrapper to return the target state to the conditional transition function.
just :: Typeable state => state -> IO (Maybe MachineState)
just = pure . Just . toMachineState

-- | A wrapper to return Nothing to the conditional transition function.
nothing :: IO (Maybe MachineState)
nothing = pure Nothing
