import           Universum
import           Control.Concurrent
import           Control.Actor
import           Control.StateMachine

main :: IO ()
main = stateMachinExemple

actorExemple :: IO ()
actorExemple = do
    actor1 <- makeActor handlers
    actor2 <- makeActor handlers
    notify actor2 $ Ping actor1 10
    threadDelay 10000

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
    otherwiseMath $ \_ -> putTextLn "\nActor success!\n"

stateMachinExemple :: IO ()
stateMachinExemple = do
    putTextLn ""
    sm <- runStateMachine Off $ do
        addTransition On  TakeOff Off
        addTransition Off TakeOn  On

        entryDo On  $ putTextLn "Now the room is bright."
        entryDo Off $ putTextLn "Now the room is dark."

    emit sm TakeOn
    emit sm TakeOn
    emit sm TakeOff

    threadDelay 10000

data On  = On
data Off = Off

data TakeOn = TakeOn
data TakeOff = TakeOff


