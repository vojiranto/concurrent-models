module Control.StateMachine.Domain where

import           Universum hiding (head)
import           Data.Typeable
import           Data.Dynamic
import           Universum.Unsafe
import qualified Data.Text     as T

newtype MachineState = MachineState TypeRep deriving (Eq, Ord, Show)
newtype MachineEvent = MachineEvent Dynamic
newtype EventType    = EventType    TypeRep deriving (Eq, Ord, Show)

toMachineState :: Typeable a => a -> MachineState
toMachineState = MachineState . typeOf

toMachineEvent :: Typeable a => a -> MachineEvent
toMachineEvent = MachineEvent . toDyn

fromMachineEvent :: Typeable a => MachineEvent -> a 
fromMachineEvent (MachineEvent event) = fromJust . fromDynamic $ event

eventToType :: MachineEvent -> EventType
eventToType (MachineEvent event) = EventType (dynTypeRep event)

actionToType :: Typeable a => (a -> IO ()) -> EventType
actionToType action = EventType (head . snd . splitTyConApp . typeOf $ action)

conditionToType :: Typeable a => (a -> IO (Maybe MachineState)) -> EventType
conditionToType action = EventType (head . snd . splitTyConApp . typeOf $ action)
