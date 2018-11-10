{-# Language DeriveFunctor    #-}
{-# Language FlexibleContexts #-}

module Control.Actor.Handler.Language where

import           Universum
import           Control.Monad.Free
import           Control.Actor.Message

type ActorHandler = ActorMsg -> IO ()

data HandlerF next where
    MakeHandler :: MsgType -> ActorHandler -> (() -> next)->  HandlerF next
    deriving (Functor)

type HandlerL = Free HandlerF

math :: Typeable a => (a -> IO ()) -> HandlerL ()
math f = liftF $ MakeHandler (fromDataToMsgType f) (f . fromActorMsg) id

otherwiseMath :: ActorHandler -> HandlerL ()
otherwiseMath f = liftF $ MakeHandler otherwiseType f id