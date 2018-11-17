module Control.Actor.Message
    ( ActorMessage
    , TypeRep
    , MessageType
    , otherwiseType
    , toActorMessage
    , fromActorMessage
    , fromDataToMessageType
    , fromActorMessageToType
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
otherwiseType = fromDataToMessageType Otherwise

toActorMessage :: Typeable a => a -> ActorMessage
toActorMessage = ActorMessage . toDyn

fromActorMessage :: Typeable a => ActorMessage -> a
fromActorMessage (ActorMessage message) = fromJust . fromDynamic $ message

fromActorMessageToType :: ActorMessage -> MessageType
fromActorMessageToType (ActorMessage message) = MessageType . dynTypeRep $ message

fromDataToMessageType :: Typeable a => a -> MessageType
fromDataToMessageType = MessageType . typeOf

fromActionToMessageType :: Typeable a => (a -> IO ()) -> MessageType
fromActionToMessageType = MessageType . head . snd . splitTyConApp . typeOf

instance Describe ActorMessage where
    describe (ActorMessage a) = "[message " <> show (dynTypeRep a) <> "]"

instance Describe MessageType where
    describe (MessageType a) = "[message " <> show a <> "]"
    