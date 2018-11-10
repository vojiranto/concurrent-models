{-# Language DeriveFunctor #-}
module Control.StateMachine.Language where

import           Universum
import           Control.Monad.Free
import           Control.Actor.Message

type MachineState = TypeRep
type MachineEvent = ActorMessage

data StateMachineF next where
    AddState        :: MachineState -> (() -> next) -> StateMachineF next
    SetInitialState :: MachineState -> (() -> next) -> StateMachineF next
    SetFinishState  :: MachineState -> (() -> next) -> StateMachineF next
    AddTransition   :: MachineState -> MachineEvent -> MachineState -> (() -> next) -> StateMachineF next
    EntryDo         :: MachineState -> IO () -> (() -> next) -> StateMachineF next
    ExitDo          :: MachineState -> IO () -> (() -> next) -> StateMachineF next
    deriving (Functor)

type StateMachineL = Free StateMachineF

addState :: MachineState -> StateMachineL ()
addState state = liftF $ AddState state id

setInitialState ::  MachineState -> StateMachineL ()
setInitialState state = liftF $ SetInitialState state id

setFinishState :: MachineState -> StateMachineL ()
setFinishState state = liftF $ SetFinishState state id

addTransition :: MachineState -> MachineEvent -> MachineState -> StateMachineL ()
addTransition state1 event state2 = liftF $ AddTransition state1 event state2 id

entryDo :: MachineState -> IO () -> StateMachineL ()
entryDo state io = liftF $ EntryDo state io id

exitDo :: MachineState -> IO () -> StateMachineL ()
exitDo state io = liftF $ ExitDo state io id