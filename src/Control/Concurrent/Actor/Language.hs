{-# Language DeriveFunctor    #-}
{-# Language FlexibleContexts #-}

module Control.Concurrent.Actor.Language where

import           Universum
import           Data.This
import qualified Data.Map as M
import           Control.Monad.Free
import           Control.Concurrent.Actor.Message
import           Control.Concurrent.STM.TChan
import           Control.Concurrent hiding (MVar, putMVar, takeMVar, newMVar)

data Actor = Actor (TChan ActorMessage) ThreadId

type HandlerMap = M.Map MessageType (ActorMessage -> IO ())

data ActorF next where
    Math :: MessageType -> (ActorMessage -> IO ()) -> (() -> next) -> ActorF next
    This :: (Actor -> next) -> ActorF next
    deriving (Functor)

type ActorL = Free ActorF


instance This ActorL Actor where
    -- | Return link of current FSM.
    this = liftF $ This id

-- | Add handler for message wich known type.
math :: Typeable a => (a -> IO ()) -> ActorL ()
math f = liftF $ Math (fromActionToMessageType f) (f . fromActorMessageUnsafe) id

-- | Add handler for processing messages with other types.
otherwiseMath :: (ActorMessage -> IO ()) -> ActorL ()
otherwiseMath f = liftF $ Math otherwiseType f id