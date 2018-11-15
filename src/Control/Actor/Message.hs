module Control.Actor.Message
    ( ActorMessage
    , TypeRep
    , MessageType
    , otherwiseType
    , toActorMessage
    , fromActorMessage
    , fromDataToMessageType
    , fromActorMessageToType
    ) where

import           Universum
import           Universum.Unsafe
import           Data.Dynamic
import           Data.Typeable
import qualified Data.Text     as T

type ActorMessage = Dynamic
type MessageType  = Text

otherwiseType :: MessageType
otherwiseType = "otherwise"

toActorMessage :: Typeable a => a -> ActorMessage
toActorMessage = toDyn

fromActorMessage :: Typeable a => ActorMessage -> a
fromActorMessage = fromJust . fromDynamic

fromActorMessageToType :: ActorMessage -> MessageType
fromActorMessageToType = T.pack . takeWhile (/= ' ') . show . dynTypeRep

fromDataToMessageType :: Typeable a => a -> MessageType
fromDataToMessageType = T.pack . takeWhile (/= ' ') . show . typeOf
