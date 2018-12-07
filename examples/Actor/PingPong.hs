module Actor.PingPong where

import           Universum

import           Control.Concurrent.Model
import           Control.Concurrent.Flag        -- To report about successful completion.
import           Control.Concurrent.Loger
import           Control.Concurrent.Actor

-- You can send different types of messages to actors.
-- If there is no suitable handler, then it will simply be droped.
data Ping = Ping Actor Int
data Pong = Pong Actor Int

actorPingPong :: IO ()
actorPingPong = do
    success <- newFlag
    pingPong success
    wait success

pingPong :: Flag -> IO ()
pingPong success = do 
    actor1 <- pinger success
    actor2 <- pinger success
    notify actor2 $ Ping actor1 10

pinger :: Flag -> IO Actor
pinger success = runActor logOff $ do
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
