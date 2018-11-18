module Actor.PingPong where

import           Universum

import           Data.Flag        -- To report about successful completion.
import           Control.Loger
import           Control.Actor

-- You can send different types of messages to actors.
-- If there is no suitable handler, then it will simply be droped.
data Ping = Ping Actor Int
data Pong = Pong Actor Int

actorPingPong :: IO ()
actorPingPong = do
    success <- newFlag
    actor1  <- makeActor logOff $ \link -> do
        math $ ping success link
        math $ pong success link

    actor2  <- makeActor logOff $ \link -> do
        math $ ping success link
        math $ pong success link

    notify actor2 $ Ping actor1 10
    wait success

-- Handlers are ordinary Haskel functions.
ping :: Flag -> Actor -> Ping -> IO ()
ping success _    (Ping _     0) = liftFlag success
ping _       link (Ping actor n) = notify actor $ Pong link (n-1)

pong :: Flag -> Actor -> Pong -> IO ()
pong success _    (Pong _     0) = liftFlag success
pong _       link (Pong actor n) = notify actor $ Ping link (n-1)
