{-# Language DeriveFunctor    #-}
{-# Language FlexibleContexts #-}

module Control.Actor.Language where

import           Universum
import           Control.Monad.Free
import           Control.Actor.Message
import           Control.Concurrent.STM.TChan
import           Control.Concurrent hiding (MVar, putMVar, takeMVar, newMVar)


data Actor = Actor (TChan ActorMessage) ThreadId

data ActorF next where
    Math :: MessageType -> (ActorMessage -> IO ()) -> (() -> next)->  ActorF next
    deriving (Functor)

type ActorL = Free ActorF
-- | Add handler for message wich known type.
math :: Typeable a => (a -> IO ()) -> ActorL ()
math f = liftF $ Math (fromActionToMessageType f) (f . fromActorMessageUnsafe) id

-- | Add handler for processing messages with other types.
otherwiseMath :: (ActorMessage -> IO ()) -> ActorL ()
otherwiseMath f = liftF $ Math otherwiseType f id