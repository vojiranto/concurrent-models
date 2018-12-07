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

import           Control.Concurrent.Prelude
import           Control.Concurrent.Model.Data.Event

data    Otherwise    = Otherwise 

otherwiseType :: EventType
otherwiseType = toType Otherwise

