import           Universum
import           Test.Hspec.Extra
import           Control.Concurrent
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
    qSem <- newQSem 1
    actor1 <- makeActor (handlers qSem)
    actor2 <- makeActor (handlers qSem)
    notify actor2 $ Ping actor1 10
    waitQSem qSem

data Ping = Ping Actor Int
data Pong = Pong Actor Int

ping :: QSem -> Actor -> Ping -> IO ()
ping qSem _      (Ping _     0) = signalQSem qSem
ping _    myLink (Ping actor n) = notify actor $ Pong myLink (n-1)

pong :: QSem -> Actor -> Pong -> IO ()
pong qSem _      (Pong _     0) = signalQSem qSem
pong _    myLink (Pong actor n) = notify actor $ Ping myLink (n-1)

handlers :: QSem -> Actor -> HandlerL ()
handlers qSem myLink = do
    math $ ping qSem myLink
    math $ pong qSem myLink

stateMachinTest1 :: IO Bool
stateMachinTest1 = finishFor 1000 $ do
    qSem <- newQSem 1
    sm   <- runStateMachine Off $ do
        addTransition On   TakeOff Off
        entryDo       On $ signalQSem qSem
    emit sm TakeOn
    waitQSem qSem

stateMachinTest2 :: IO Bool
stateMachinTest2 = finishFor 1000 $ do
    qSem <- newQSem 1
    sm   <- runStateMachine Off $ do
        addConditionalTransition Off $
            \pressing -> if pressing == StrongPress then just On else nothing 
        addConditionalTransition On $
            \pressing -> if pressing == StrongPress then just Off else nothing

        transitionDo On Off $ \(_ :: Press) -> signalQSem qSem

    emit sm StrongPress
    emit sm StrongPress
    waitQSem qSem

data Press = StrongPress | WeaklyPress deriving Eq

data On  = On
data Off = Off

data TakeOn = TakeOn
data TakeOff = TakeOff


