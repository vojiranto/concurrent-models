module Control.StateMachine.Domain where

import           Universum
import           Data.Typeable

newtype MachineState = MachineState TypeRep deriving (Eq, Ord, Show)
newtype MachineEvent = MachineEvent TypeRep deriving (Eq, Ord, Show)

toMachineState :: Typeable a => a -> MachineState
toMachineState = MachineState . typeOf

toMachineEvent :: Typeable a => a -> MachineEvent
toMachineEvent = MachineEvent . typeOf