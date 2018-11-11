{-# Language DeriveFunctor #-}
module Control.StateMachine.Language where

import           Universum
import           Control.Monad.Free
import           Control.StateMachine.Domain

data StateMachineF next where
    SetFinishState  :: MachineState -> (() -> next) -> StateMachineF next
    AddTransition   :: MachineState -> MachineEvent -> MachineState -> (() -> next) -> StateMachineF next
    EntryDo         :: MachineState -> IO () -> (() -> next) -> StateMachineF next
    ExitDo          :: MachineState -> IO () -> (() -> next) -> StateMachineF next
    deriving (Functor)

type StateMachineL = Free StateMachineF

setFinishState :: Typeable a => a -> StateMachineL ()
setFinishState state = liftF $ SetFinishState (toMachineState state) id

addTransition :: (Typeable a, Typeable b, Typeable c) => a -> b -> c -> StateMachineL ()
addTransition state1 event state2 =
    liftF $ AddTransition (toMachineState state1) (toMachineEvent event) (toMachineState state2) id

entryDo :: Typeable a => a -> IO () -> StateMachineL ()
entryDo state io = liftF $ EntryDo (toMachineState state) io id

exitDo :: Typeable a => a -> IO () -> StateMachineL ()
exitDo state io = liftF $ ExitDo (toMachineState state) io id