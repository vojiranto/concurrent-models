module Control.Concurrent.Model.Core.Data.Event
    ( Event
    , EventType
    , eventType
    , ToType(..)
    , toEvent
    , fromEvent
    , fromEventUnsafe
    , eventToType
    , actionToType
    , rawDataToType
    , proxyToType
    ) where

import           Control.Concurrent.Prelude
import           Control.Concurrent.Model.Core.Data.Describe

newtype Event        = Event        Dynamic
newtype EventType    = EventType    TypeRep deriving (Eq, Ord)

eventType :: TypeRep -> EventType
eventType = EventType

toEvent :: Typeable a => a -> Event
toEvent = Event . toDyn

fromEvent :: Typeable a => Event -> Maybe a
fromEvent (Event message) = fromDynamic message

fromEventUnsafe :: Typeable a => Event -> a 
fromEventUnsafe (Event event) = fromJust . fromDynamic $ event

eventToType :: Event -> EventType
eventToType (Event event) = EventType (dynTypeRep event)

actionToType :: Typeable a => (a -> IO ()) -> EventType
actionToType = EventType . head . snd . splitTyConApp . typeOf

proxyToType :: Typeable a => Proxy a -> EventType
proxyToType = EventType . head . snd . splitTyConApp . typeOf

rawDataToType :: Typeable a => a -> EventType
rawDataToType = EventType . typeOf

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


