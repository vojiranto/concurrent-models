{-# Language DeriveFunctor    #-}
{-# Language FlexibleContexts #-}
{-# Language TemplateHaskell  #-}

module Control.Concurrent.Model.Actor.Language where

import           Control.Concurrent.Prelude

import qualified Data.Map as M
import           Language.Haskell.TH.MakeFunctor
import           Control.Concurrent.Model.Core
import           Control.Concurrent.Model.Actor.Message

data Actor = Actor (TChan Event) ThreadId TextId (MVar Bool)

getChan :: Actor -> TChan Event
getChan (Actor chan _ _ _) = chan

getTreadId :: Actor -> ThreadId
getTreadId (Actor _ threadId _ _) = threadId

setIsDead :: Actor -> IO ()
setIsDead (Actor _ _ _ liveFlag) = void $ swapMVar liveFlag False

instance HaveTextId Actor where
    getTextId (Actor _ _ textId _) = textId 

recieveMessage :: Actor -> IO Event
recieveMessage (Actor chan _ _ _) = atomically $ readTChan chan

type HandlerMap = M.Map EventType (Event -> IO ())

data ActorF next where
    Math    :: EventType -> (Event -> IO ()) -> (() -> next) -> ActorF next
    This    :: (Actor -> next) -> ActorF next
    LiftIO  :: IO a -> (a -> next) -> ActorF next
    ToLog   :: LogLevel -> Text -> (() -> next) ->  ActorF next
    GetLoger :: (Loger -> next) ->  ActorF next
makeFunctorInstance ''ActorF

type ActorL = Free ActorF

instance Logers ActorL where
    toLog logLevel txt = liftF $ ToLog logLevel txt id
    getLoger = liftF $ GetLoger id

instance MonadIO ActorL where
    liftIO action = liftF $ LiftIO action id

instance This ActorL Actor where
    -- | Return link of current FSM.
    this = liftF $ This id
    isLive (Actor _ _ _ liveFlag) = readMVar liveFlag

instance Typeable a => Math (a -> IO ()) ActorL where
    math f = liftF $ Math (fromActionToMessageType f) (f . fromEventUnsafe) id

-- | Add handler for processing messages with other types.
otherwiseMath :: (Event -> IO ()) -> ActorL ()
otherwiseMath f = liftF $ Math otherwiseType f id