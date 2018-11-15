module Control.StateMachine.Domain where

import           Universum hiding (head, ToText(..))
import           Data.Typeable
import           Data.Dynamic
import           Universum.Unsafe

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

class ToText a where
    toText :: a -> Text

instance ToText MachineEvent where
    toText (MachineEvent a) = show (dynTypeRep a)

instance ToText MachineState where
    toText (MachineState a) = show a

instance ToText EventType where
    toText (EventType a) = show a