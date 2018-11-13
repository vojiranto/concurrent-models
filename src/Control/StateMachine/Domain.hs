module Control.StateMachine.Domain where

import           Universum
import           Data.Typeable
import           Data.Dynamic
import           Universum.Unsafe
import qualified Data.Text     as T

newtype MachineState = MachineState Text deriving (Eq, Ord, Show)
newtype MachineEvent = MachineEvent Dynamic
newtype EventType    = EventType    Text deriving (Eq, Ord, Show)

toMachineState :: Typeable a => a -> MachineState
toMachineState = MachineState . show . typeOf

toMachineEvent :: Typeable a => a -> MachineEvent
toMachineEvent = MachineEvent . toDyn

fromMachineEvent :: Typeable a => MachineEvent -> a 
fromMachineEvent (MachineEvent event) = fromJust . fromDynamic $ event

eventToType :: MachineEvent -> EventType
eventToType (MachineEvent event) = EventType (show . dynTypeRep $ event)

actionToType :: Typeable a => (a -> IO ()) -> EventType
actionToType action = EventType (T.pack . takeWhile (/= ' ') . show . typeOf $ action)
