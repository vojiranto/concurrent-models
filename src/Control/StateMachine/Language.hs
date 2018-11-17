{-# Language TemplateHaskell       #-}
{-# Language TypeSynonymInstances  #-}
{-# Language FlexibleInstances     #-}
module Control.StateMachine.Language
    ( StateMachineF(..)
    , StateMachineL
    , StateMachine(..)
    , getMyLink
    , groupStates
    , setFinishState
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
import           Control.StateMachine.Domain

data StateMachine = StateMachine (Chan Event) (MVar MachineState)

data StateMachineF next where
    LiftIO                      :: IO a -> (a -> next) -> StateMachineF next
    GetMyLink                   :: (StateMachine -> next) -> StateMachineF next
    -- Construction of state mashine struct
    GroupStates                 :: MachineState -> [MachineState] -> (() -> next) -> StateMachineF next
    SetFinishState              :: MachineState -> (() -> next) -> StateMachineF next
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

getMyLink :: StateMachineL StateMachine
getMyLink = liftF $ GetMyLink id

setFinishState :: Typeable state => state -> StateMachineL ()
setFinishState finishState = liftF $
    SetFinishState (toMachineState finishState) id

groupStates :: Typeable group => group -> [MachineState] -> StateMachineL ()
groupStates groupName states = liftF $ GroupStates (toMachineState groupName) states id

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

exitWithEventDo :: (Typeable state, Typeable event) => state -> (event -> IO ()) -> StateMachineL ()
exitWithEventDo oldState action = liftF $ ExitWithEventDo
    (toMachineState oldState) (actionToType action) (action . fromMachineEvent) id

exitDo :: Typeable state => state -> IO () -> StateMachineL ()
exitDo oldState action = liftF $ ExitDo (toMachineState oldState) action id

just :: Typeable state => state -> IO (Maybe MachineState)
just = pure . Just . toMachineState

nothing :: IO (Maybe MachineState)
nothing = pure Nothing
