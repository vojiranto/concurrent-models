{-# Language TemplateHaskell #-}
import           Universum
import           Test.Hspec.Extra

import           Data.Flag
import           Control.Loger
import           Control.Actor
import           Control.StateMachine

actorPingPongTest :: IO Bool
actorPingPongTest = finishFor 1000 $ do
    success <- newFlag
    actor1  <- makeActor (handlers success)
    actor2  <- makeActor (handlers success)
    notify actor2 $ Ping actor1 10
    wait success

data Ping = Ping Actor Int
data Pong = Pong Actor Int

ping :: Flag -> Actor -> Ping -> IO ()
ping sem  _      (Ping _     0) = liftFlag sem
ping _    link (Ping actor n) = notify actor $ Pong link (n-1)

pong :: Flag -> Actor -> Pong -> IO ()
pong sem  _      (Pong _     0) = liftFlag sem
pong _    link (Pong actor n) = notify actor $ Ping link (n-1)

handlers :: Flag -> Actor -> HandlerL ()
handlers qSem link = do
    math $ ping qSem link
    math $ pong qSem link

makeStates ["On", "Off"]
makeEvents ["TakeOn"]

stateMachinTest1 :: IO Bool
stateMachinTest1 = finishFor 100 $ do
    success <- newFlag
    sm  <- runStateMachine logOff Off $ do
        addTransition  Off TakeOn On
        setFinishState On
        entryDo        On  $ liftFlag success
        exitDo         Off $ pure ()
        exitDo         On  $ pure ()
    emit sm TakeOn
    wait success

stateMachinTest2 :: IO Bool
stateMachinTest2 = finishFor 1000 $ do
    success <- newFlag
    sm      <- runStateMachine logOff Off $ do
        addConditionalTransition Off $
            \pressing -> if pressing == StrongPress then just On else nothing 
        addConditionalTransition On $
            \pressing -> if pressing == StrongPress then just Off else nothing

        transitionDo On Off $ \(_ :: Press) -> liftFlag success

    emit sm StrongPress
    emit sm StrongPress
    wait success

data Press = StrongPress | WeaklyPress deriving Eq

main :: IO ()
main = do
    putTextLn ""
    hspec $ do
        it "Actor ping pong test"     $ isOk actorPingPongTest
        it "Test 1 for state machine" $ isOk stateMachinTest1
        it "Test 2 for state machine" $ isOk stateMachinTest2
