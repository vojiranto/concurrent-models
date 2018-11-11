module Control.StateMachine.Domain where

import           Universum
import           Data.Typeable

newtype MachineState = MachineState TypeRep deriving (Eq, Ord)
newtype MachineEvent = MachineEvent TypeRep deriving (Eq, Ord)

toMachineState :: Typeable a => a -> MachineState
toMachineState = MachineState . typeOf

toMachineEvent :: Typeable a => a -> MachineEvent
toMachineEvent = MachineEvent . typeOf