module Data.Event where

import           Universum hiding (head)
import           Universum.Unsafe

import           Data.Typeable
import           Data.Dynamic
import           Data.Describe

newtype Event        = Event        Dynamic
newtype EventType    = EventType    TypeRep deriving (Eq, Ord)

toEvent :: Typeable a => a -> Event
toEvent = Event . toDyn

fromEvent :: Typeable a => Event -> a 
fromEvent (Event event) = fromJust . fromDynamic $ event

eventToType :: Event -> EventType
eventToType (Event event) = EventType (dynTypeRep event)

actionToType :: Typeable a => (a -> IO ()) -> EventType
actionToType action = EventType (head . snd . splitTyConApp . typeOf $ action)

instance Describe Event where
    describe (Event a) = "[event " <> show (dynTypeRep a) <> "]"

instance Describe EventType where
    describe (EventType a) = "[event " <> show a <> "]"
    