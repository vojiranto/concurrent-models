module Control.Concurrent.Model.Data.Event where

import           Control.Concurrent.Prelude
import           Control.Concurrent.Model.Data.Describe

newtype Event        = Event        Dynamic
newtype EventType    = EventType    TypeRep deriving (Eq, Ord)

toEvent :: Typeable a => a -> Event
toEvent = Event . toDyn

fromEvent :: Typeable a => Event -> Maybe a
fromEvent (Event message) = fromDynamic message

fromEventUnsafe :: Typeable a => Event -> a 
fromEventUnsafe (Event event) = fromJust . fromDynamic $ event

eventToType :: Event -> EventType
eventToType (Event event) = EventType (dynTypeRep event)

actionToType :: Typeable a => (a -> IO ()) -> EventType
actionToType action = EventType (head . snd . splitTyConApp . typeOf $ action)

instance Describe Event where
    describe (Event a) = "[event " <> show (dynTypeRep a) <> "]"

instance Describe EventType where
    describe (EventType a) = "[event " <> show a <> "]"
    
-- | You can get to know out which type the message has to select a handler.
class ToType a where
    toType :: a -> EventType

instance {-# OVERLAPS #-} ToType Event where
    toType (Event message) = EventType . dynTypeRep $ message

instance {-# OVERLAPPABLE #-} Typeable a => ToType a where
    toType = EventType . typeOf

rawDataToType :: Typeable a => a -> EventType
rawDataToType = EventType . typeOf