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
    sm1 <- runStateMachine Off $ do
        addTransition On  TakeOff Off
        addTransition Off TakeOn  On

        entryDo On     $ putTextLn "Now the room1 is bright."
        entryDo Off    $ putTextLn "Now the room1 is dark."
        staticalDo On  $ \TakeOn  -> putTextLn "The light is already on."
        staticalDo Off $ \TakeOff -> putTextLn "The light is already off."

    emit sm1 TakeOn
    emit sm1 TakeOn
    emit sm1 TakeOff
    emit sm1 TakeOff

    threadDelay 10000
    putTextLn ""
    sm2 <- runStateMachine Off $ do
        addConditionalTransition Off $
            \pressing -> if pressing == StrongPress then just On else nothing 
        addConditionalTransition On $
            \pressing -> if pressing == StrongPress then just Off else nothing

        entryDo On  $ putTextLn "Now the room2 is bright."
        entryDo Off $ putTextLn "Now the room2 is dark."

    emit sm2 StrongPress
    emit sm2 WeaklyPress
    emit sm2 StrongPress
    threadDelay 10000

data Press = StrongPress | WeaklyPress deriving Eq

data On  = On
data Off = Off

data TakeOn = TakeOn
data TakeOff = TakeOff


