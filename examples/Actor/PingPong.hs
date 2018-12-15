module Actor.PingPong where

import           Universum

import           Control.Concurrent.Model
import           Control.Concurrent.Flag        -- To report about successful completion.

-- You can send different types of messages to actors.
-- If there is no suitable handler, then it will simply be droped.
data Ping = Ping Actor Int
data Pong = Pong Actor Int

actorPingPong :: Loger -> IO ()
actorPingPong loger = do
    success <- newFlag
    pingPong loger success
    wait success

pingPong :: Loger -> Flag -> IO ()
pingPong loger success = do 
    actor1 <- pinger loger success
    actor2 <- pinger loger success
    notify actor2 $ Ping actor1 10

pinger :: Loger -> Flag -> IO Actor
pinger loger success = runActor loger $ do
    link <- this
    math $ ping success link
    math $ pong success link

-- Handlers are ordinary Haskel functions.
ping :: Flag -> Actor -> Ping -> IO ()
ping success _    (Ping _     0) = liftFlag success
ping _       link (Ping actor n) = notify actor $ Pong link (n-1)

pong :: Flag -> Actor -> Pong -> IO ()
pong success _    (Pong _     0) = liftFlag success
pong _       link (Pong actor n) = notify actor $ Ping link (n-1)
