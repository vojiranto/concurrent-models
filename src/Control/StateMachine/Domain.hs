{-# Language MultiParamTypeClasses #-}
{-# Language FlexibleInstances     #-}
module Control.StateMachine.Domain where

import           Universum hiding (head)
import           Universum.Unsafe

import           Data.Typeable
import           Data.Dynamic
import           Data.Describe

data Event = FastEvent MachineEvent | WaitEvent MachineEvent (MVar ())

getEvent :: Event -> IO MachineEvent
getEvent (FastEvent event)     = pure event
getEvent (WaitEvent event var) = putMVar var () >> pure event

newtype MachineState = MachineState TypeRep deriving (Eq, Ord)
newtype MachineEvent = MachineEvent Dynamic
newtype EventType    = EventType    TypeRep deriving (Eq, Ord)

is :: Typeable a => a -> MachineState -> Bool
a `is` b = toMachineState a == b

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

(<:) :: Typeable a => a -> [MachineState] -> [MachineState]
a <: b = toMachineState a : b
