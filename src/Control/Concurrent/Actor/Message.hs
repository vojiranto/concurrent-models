{-# Language FlexibleInstances #-}
module Control.Concurrent.Actor.Message
    ( Event
    , ToType(..)
    , TypeRep
    , EventType
    , otherwiseType
    , toEvent
    , fromEvent
    , fromEventUnsafe
    , fromActionToMessageType
    ) where

import           Universum hiding (head)
import           Universum.Unsafe
import           Data.Typeable
import           Control.Concurrent.Model.Data.Event

data    Otherwise    = Otherwise 

otherwiseType :: EventType
otherwiseType = toType Otherwise

fromActionToMessageType :: Typeable a => (a -> IO ()) -> EventType
fromActionToMessageType = EventType . head . snd . splitTyConApp . typeOf    