{-# Language MultiParamTypeClasses #-}
{-# Language FlexibleInstances     #-}
module Control.Concurrent.StateMachine.Domain where

import           Universum hiding (head)
import           Universum.Unsafe
import           Data.Typeable

import           Control.Concurrent.Flag
import           Control.Concurrent.Core.Data

data Transition = Transition MachineState MachineState
data PackagedEvent = FastEvent Event | WaitEvent Event Flag

getMachineEvent :: PackagedEvent -> IO (Event, Maybe Flag)
getMachineEvent (FastEvent event)      = pure (event, Nothing)
getMachineEvent (WaitEvent event flag) = pure (event, Just flag)

newtype MachineState = MachineState TypeRep deriving (Eq, Ord)

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

conditionToType :: Typeable a => (a -> IO (Maybe MachineState)) -> EventType
conditionToType action = EventType (head . snd . splitTyConApp . typeOf $ action)

instance Describe MachineState where
    describe (MachineState a) = "[state " <> show a <> "]"

infixr 6  <:

-- | Add new state to list.
(<:) :: Typeable a => a -> [MachineState] -> [MachineState]
a <: b = toMachineState a : b
