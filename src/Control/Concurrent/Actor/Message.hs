{-# Language FlexibleInstances #-}
module Control.Concurrent.Actor.Message
    ( ActorMessage
    , ToType(..)
    , TypeRep
    , MessageType
    , otherwiseType
    , toActorMessage
    , fromActorMessage
    , fromActorMessageUnsafe
    , fromActionToMessageType
    ) where

import           Universum hiding (head)
import           Universum.Unsafe
import           Data.Dynamic
import           Data.Typeable
import           Data.Describe

data    Otherwise    = Otherwise 
newtype ActorMessage = ActorMessage Dynamic
newtype MessageType  = MessageType  TypeRep deriving (Ord, Eq)

otherwiseType :: MessageType
otherwiseType = toType Otherwise

toActorMessage :: Typeable a => a -> ActorMessage
toActorMessage = ActorMessage . toDyn

-- | Unpack actor message.
fromActorMessage :: Typeable a => ActorMessage -> Maybe a
fromActorMessage (ActorMessage message) = fromDynamic message

fromActorMessageUnsafe :: Typeable a => ActorMessage -> a
fromActorMessageUnsafe (ActorMessage message) = fromJust . fromDynamic $ message

fromActionToMessageType :: Typeable a => (a -> IO ()) -> MessageType
fromActionToMessageType = MessageType . head . snd . splitTyConApp . typeOf

-- | You can get to know out which type the message has to select a handler.
class ToType a where
    toType :: a -> MessageType

instance {-# OVERLAPS #-} ToType ActorMessage where
    toType (ActorMessage message) = MessageType . dynTypeRep $ message

instance {-# OVERLAPPABLE #-} Typeable a => ToType a where
    toType = MessageType . typeOf

instance Describe ActorMessage where
    describe (ActorMessage a) = "[message " <> show (dynTypeRep a) <> "]"

instance Describe MessageType where
    describe (MessageType a) = "[message " <> show a <> "]"
    