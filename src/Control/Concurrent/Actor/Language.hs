{-# Language DeriveFunctor    #-}
{-# Language FlexibleContexts #-}
{-# Language TemplateHaskell  #-}

module Control.Concurrent.Actor.Language where

import           Universum
import           Data.This
import qualified Data.Map as M
import           Language.Haskell.TH.MakeFunctor
import           Control.Monad.Free
import           Control.Concurrent.Actor.Message
import           Control.Concurrent.STM.TChan
import           Control.Concurrent hiding (MVar, putMVar, takeMVar, newMVar)

data Actor = Actor (TChan ActorMessage) ThreadId

recieveMessage :: Actor -> IO ActorMessage
recieveMessage (Actor chan _) = atomically $ readTChan chan

type HandlerMap = M.Map MessageType (ActorMessage -> IO ())

data ActorF next where
    Math    :: MessageType -> (ActorMessage -> IO ()) -> (() -> next) -> ActorF next
    This    :: (Actor -> next) -> ActorF next
    LiftIO  :: IO a -> (a -> next) -> ActorF next
makeFunctorInstance ''ActorF

type ActorL = Free ActorF

instance MonadIO ActorL where
    liftIO action = liftF $ LiftIO action id

instance This ActorL Actor where
    -- | Return link of current FSM.
    this = liftF $ This id

-- | Add handler for message wich known type.
math :: Typeable a => (a -> IO ()) -> ActorL ()
math f = liftF $ Math (fromActionToMessageType f) (f . fromActorMessageUnsafe) id

-- | Add handler for processing messages with other types.
otherwiseMath :: (ActorMessage -> IO ()) -> ActorL ()
otherwiseMath f = liftF $ Math otherwiseType f id