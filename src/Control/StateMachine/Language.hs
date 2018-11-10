{-# Language DeriveFunctor #-}
module Control.StateMachine.Language where

import           Universum
import           Control.Monad.Free
import           Data.Typeable

type MachineState = TypeRep
type MachineEvent = TypeRep

data StateMachineF next where
    AddState        :: MachineState -> (() -> next) -> StateMachineF next
    SetInitialState :: MachineState -> (() -> next) -> StateMachineF next
    SetFinishState  :: MachineState -> (() -> next) -> StateMachineF next
    AddTransition   :: MachineState -> MachineEvent -> MachineState -> (() -> next) -> StateMachineF next
    EntryDo         :: MachineState -> IO () -> (() -> next) -> StateMachineF next
    ExitDo          :: MachineState -> IO () -> (() -> next) -> StateMachineF next
    deriving (Functor)

type StateMachineL = Free StateMachineF

addState :: Typeable a => a -> StateMachineL ()
addState state = liftF $ AddState (typeOf state) id

setInitialState :: Typeable a => a -> StateMachineL ()
setInitialState state = liftF $ SetInitialState (typeOf state) id

setFinishState :: Typeable a => a -> StateMachineL ()
setFinishState state = liftF $ SetFinishState (typeOf state) id

addTransition :: (Typeable a, Typeable b, Typeable c) => a -> b -> c -> StateMachineL ()
addTransition state1 event state2 = liftF $ AddTransition (typeOf state1) (typeOf event) (typeOf state2) id

entryDo :: Typeable a => a -> IO () -> StateMachineL ()
entryDo state io = liftF $ EntryDo (typeOf state) io id

exitDo :: Typeable a => a -> IO () -> StateMachineL ()
exitDo state io = liftF $ ExitDo (typeOf state) io id