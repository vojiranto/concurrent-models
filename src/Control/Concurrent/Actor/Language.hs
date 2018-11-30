{-# Language DeriveFunctor    #-}
{-# Language FlexibleContexts #-}
{-# Language TemplateHaskell  #-}

module Control.Concurrent.Actor.Language where

import           Universum
import           Data.This
import qualified Data.Map as M
import           Control.Concurrent.Math
import           Language.Haskell.TH.MakeFunctor
import           Control.Monad.Free
import           Control.Concurrent.Actor.Message
import           Control.Concurrent.STM.TChan
import           Control.Concurrent hiding (MVar, putMVar, takeMVar, newMVar)

data Actor = Actor (TChan Event) ThreadId

recieveMessage :: Actor -> IO Event
recieveMessage (Actor chan _) = atomically $ readTChan chan

type HandlerMap = M.Map EventType (Event -> IO ())

data ActorF next where
    Math    :: EventType -> (Event -> IO ()) -> (() -> next) -> ActorF next
    This    :: (Actor -> next) -> ActorF next
    LiftIO  :: IO a -> (a -> next) -> ActorF next
makeFunctorInstance ''ActorF

type ActorL = Free ActorF

instance MonadIO ActorL where
    liftIO action = liftF $ LiftIO action id

instance This ActorL Actor where
    -- | Return link of current FSM.
    this = liftF $ This id

instance Typeable a => Math (a -> IO ()) ActorL where
    math f = liftF $ Math (fromActionToMessageType f) (f . fromEventUnsafe) id

-- | Add handler for processing messages with other types.
otherwiseMath :: (Event -> IO ()) -> ActorL ()
otherwiseMath f = liftF $ Math otherwiseType f id