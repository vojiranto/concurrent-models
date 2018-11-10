import           Universum
import           Control.Concurrent
import           Control.Actor

data Ping = Ping Actor Int
data Pong = Pong Actor Int

ping :: Actor -> Ping -> IO ()
ping myLink (Ping actor 0) = notify actor ("!@&#$!" :: Text)
ping myLink (Ping actor n) = notify actor $ Pong myLink (n-1)

pong :: Actor -> Pong -> IO ()
pong myLink (Pong actor 0) = notify actor ("!@&#$!" :: Text)
pong myLink (Pong actor n) = notify actor $ Ping myLink (n-1)

handlers :: Actor -> HandlerL ()
handlers myLink = do
    math          $ ping myLink
    math          $ pong myLink
    otherwiseMath $ \_ -> putTextLn "\nSuccess!\n"

main :: IO ()
main = do
    actor1 <- mkActor handlers
    actor2 <- mkActor handlers
    notify actor2 $ Ping actor1 10
    threadDelay 10000