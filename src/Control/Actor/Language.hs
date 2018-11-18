{-# Language DeriveFunctor    #-}
{-# Language FlexibleContexts #-}

module Control.Actor.Language where

import           Universum
import           Control.Monad.Free
import           Control.Actor.Message

type ActorHandler = ActorMessage -> IO ()

data ActorF next where
    Math :: MessageType -> ActorHandler -> (() -> next)->  ActorF next
    deriving (Functor)

type ActorL = Free ActorF

math :: Typeable a => (a -> IO ()) -> ActorL ()
math f = liftF $ Math (fromActionToMessageType f) (f . fromActorMessage) id

otherwiseMath :: ActorHandler -> ActorL ()
otherwiseMath f = liftF $ Math otherwiseType f id