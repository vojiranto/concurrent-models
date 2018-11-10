module Control.Actor.Message
    ( ActorMsg
    , TypeRep(..)
    , MsgType
    , otherwiseType
    , toActorMsg
    , fromActorMsg
    , fromDataToMsgType
    , fromActorMsgToType
    ) where

import           Universum
import           Universum.Unsafe
import           Data.Dynamic
import           Data.Typeable
import qualified Data.Text     as T

type ActorMsg = Dynamic
type MsgType  = Text

otherwiseType :: MsgType
otherwiseType = "otherwise"

toActorMsg :: Typeable a => a -> ActorMsg
toActorMsg = toDyn

fromActorMsg :: Typeable a => ActorMsg -> a
fromActorMsg = fromJust . fromDynamic

fromActorMsgToType :: ActorMsg -> MsgType
fromActorMsgToType = T.pack . takeWhile (/= ' ') . show . dynTypeRep

fromDataToMsgType :: Typeable a => a -> MsgType
fromDataToMsgType = T.pack . takeWhile (/= ' ') . show . typeOf
