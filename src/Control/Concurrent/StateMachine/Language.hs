{-# Language TemplateHaskell       #-}
module Control.Concurrent.StateMachine.Language
    ( StateMachineF(..)
    , StateMachineL
    , StateMachine(..)
    , Acception(..)
    , this
    , groupStates
    , addFinalState
    , addTransition
    , addConditionalTransition
    , mathS
    , takeState
    , just
    , nothing
    , ifE
    , (>->)
    , (>?>)
    ) where

import           Universum
import           Language.Haskell.TH.MakeFunctor
import           Control.Concurrent.Chan
import           Control.Monad.Free
import           Data.Event
import           Data.This
import           Control.Concurrent.Math
import           Control.Concurrent.StateMachine.Domain

data StateMachine = StateMachine (Chan PackagedEvent) (MVar MachineState)

-- | Take current state of the FSN.
takeState :: StateMachine -> IO MachineState
takeState (StateMachine _ stateVar) = readMVar stateVar

data StateMachineF next where
    LiftIO                      :: IO a -> (a -> next) -> StateMachineF next
    This                        :: (StateMachine -> next) -> StateMachineF next
    -- Construction of state mashine struct
    GroupStates                 :: MachineState -> [MachineState] -> (() -> next) -> StateMachineF next
    AddFinalState               :: MachineState -> (() -> next) -> StateMachineF next
    AddTransition               :: MachineState -> Event -> MachineState -> (() -> next) -> StateMachineF next
    AddConditionalTransition    :: MachineState -> EventType -> (Event -> IO (Maybe MachineState)) -> (() -> next) -> StateMachineF next
    -- Addition handlers to states and transitions of state mashine
    MathDo                      :: EventType -> (Event -> IO ()) -> (() -> next) -> StateMachineF next
    StaticalDo                  :: MachineState -> EventType -> (Event -> IO ()) -> (() -> next) -> StateMachineF next
    EntryDo                     :: MachineState -> IO () -> (() -> next) -> StateMachineF next
    EntryWithEventDo            :: MachineState -> EventType -> (Event -> IO ()) -> (() -> next) -> StateMachineF next
    ExitWithEventDo             :: MachineState -> EventType -> (Event -> IO ()) -> (() -> next) -> StateMachineF next
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
    liftF $ AddTransition (toMachineState state1) (toEvent event) (toMachineState state2) id

-- | Add a conditional transition, if the function returns nothing, the transition will not occur.
addConditionalTransition
    :: (Typeable state, Typeable event)
    => state -> (event -> IO (Maybe MachineState)) -> StateMachineL ()
addConditionalTransition currentState condition = liftF $ AddConditionalTransition
    (toMachineState currentState)
    (conditionToType condition)
    (condition . fromEventUnsafe)
    id

class Acception state a where
    -- | Execute the handler if the machine enters the state.
    onEntry :: state -> a -> StateMachineL ()
    -- | Execute the handler if the machine exit the state state.
    onExit  :: state -> a -> StateMachineL ()

instance Typeable state => Acception state (IO ()) where
    onEntry newState action = liftF $ EntryDo (toMachineState newState) action id
    onExit  oldState action = liftF $ ExitDo (toMachineState oldState) action id

instance (Typeable state, Typeable event) => Acception state (event -> IO ()) where
    onEntry newState action = liftF $ EntryWithEventDo
        (toMachineState newState) (actionToType action) (action . fromEventUnsafe) id

    onExit oldState action = liftF $ ExitWithEventDo
        (toMachineState oldState) (actionToType action) (action . fromEventUnsafe) id

instance Typeable event => Math (event -> IO ()) StateMachineL where 
    math action = liftF $ MathDo (actionToType action) (action . fromEventUnsafe) id

-- | If event does not cause a transition, call handler with the event.
mathS :: (Typeable state, Typeable event) => state -> (event -> IO ()) -> StateMachineL ()
mathS currentState action = liftF $ StaticalDo
    (toMachineState currentState) (actionToType action) (action . fromEventUnsafe) id

-- | A wrapper to return the target state to the conditional transition function.
just :: Typeable state => state -> IO (Maybe MachineState)
just = pure . Just . toMachineState

-- | A wrapper to return Nothing to the conditional transition function.
nothing :: IO (Maybe MachineState)
nothing = pure Nothing

ifE
    :: (Typeable state2, Typeable event, Typeable state1)
    => event -> (state1, state2) -> StateMachineL ()
ifE event (st1, st2) = addTransition st1 event st2

(>->) :: a -> b -> (a, b)
(>->) st1 st2 = (st1, st2)

(>?>) :: (Typeable state, Typeable event)
    => state -> (event -> IO (Maybe MachineState)) -> StateMachineL ()
(>?>) = addConditionalTransition
