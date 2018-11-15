import           Universum
import           Test.Hspec.Extra
import           Control.Actor
import           Control.StateMachine

main :: IO ()
main = do
    putTextLn ""
    hspec $ do
        it "Actor ping pong test"     $ isOk actorPingPongTest
        it "Test 1 for state machine" $ isOk stateMachinTest1
        it "Test 2 for state machine" $ isOk stateMachinTest2

actorPingPongTest :: IO Bool
actorPingPongTest = finishFor 1000 $ do
    sem    <- newEmptyMVar
    actor1 <- makeActor (handlers sem)
    actor2 <- makeActor (handlers sem)
    notify actor2 $ Ping actor1 10
    takeMVar sem

data Ping = Ping Actor Int
data Pong = Pong Actor Int

ping :: MVar () -> Actor -> Ping -> IO ()
ping sem  _      (Ping _     0) = putMVar sem ()
ping _    myLink (Ping actor n) = notify actor $ Pong myLink (n-1)

pong :: MVar () -> Actor -> Pong -> IO ()
pong sem  _      (Pong _     0) = putMVar sem ()
pong _    myLink (Pong actor n) = notify actor $ Ping myLink (n-1)

handlers :: MVar () -> Actor -> HandlerL ()
handlers qSem myLink = do
    math $ ping qSem myLink
    math $ pong qSem myLink

stateMachinTest1 :: IO Bool
stateMachinTest1 = finishFor 100000 $ do
    sem <- newEmptyMVar
    sm   <- runStateMachine logOff Off $ do
        addTransition  Off   TakeOn On
        addTransition  On    TakeOff Off
        setFinishState On
        entryDo       On $ putMVar sem ()
    emit sm TakeOn
    takeMVar sem

data TakeOn  = TakeOn
data TakeOff = TakeOff

stateMachinTest2 :: IO Bool
stateMachinTest2 = finishFor 1000 $ do
    sem <- newEmptyMVar
    sm   <- runStateMachine logOff Off $ do
        addConditionalTransition Off $
            \pressing -> if pressing == StrongPress then just On else nothing 
        addConditionalTransition On $
            \pressing -> if pressing == StrongPress then just Off else nothing

        transitionDo On Off $ \(_ :: Press) -> putMVar sem ()

    emit sm StrongPress
    emit sm StrongPress
    takeMVar sem

data Press = StrongPress | WeaklyPress deriving Eq

data On  = On
data Off = Off
