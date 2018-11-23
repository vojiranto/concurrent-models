{-# Language MultiParamTypeClasses #-}
{-# Language FlexibleInstances     #-}
module Control.Concurrent.StateMachine.Domain where

import           Universum hiding (head)
import           Universum.Unsafe

import           Data.Typeable
import           Data.Dynamic
import           Data.Describe
import           Data.Flag

data Transition = Transition MachineState MachineState
data Event = FastEvent MachineEvent | WaitEvent MachineEvent Flag

getEvent :: Event -> IO (MachineEvent, Maybe Flag)
getEvent (FastEvent event)     = pure (event, Nothing)
getEvent (WaitEvent event flag) = pure (event, Just flag)

newtype MachineState = MachineState TypeRep deriving (Eq, Ord)
newtype MachineEvent = MachineEvent Dynamic
newtype EventType    = EventType    TypeRep deriving (Eq, Ord)

class Is a b where
    -- | Wrapper for (==), if you want to compare the value and state of the machine.
    -- if you have two states, use (==).
    is :: a -> b -> Bool

instance Typeable a => Is a MachineState where
    a `is` b = toMachineState a == b

instance Typeable a => Is MachineState a where
    a `is` b = a == toMachineState b

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

instance Describe MachineEvent where
    describe (MachineEvent a) = "[event " <> show (dynTypeRep a) <> "]"

instance Describe MachineState where
    describe (MachineState a) = "[state " <> show a <> "]"

instance Describe EventType where
    describe (EventType a) = "[event " <> show a <> "]"

infixr 6  <:

-- | Add new state to list.
(<:) :: Typeable a => a -> [MachineState] -> [MachineState]
a <: b = toMachineState a : b
