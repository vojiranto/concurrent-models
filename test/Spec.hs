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
    actor1  <- makeActor logToConsole (handlers success)
    actor2  <- makeActor logToConsole (handlers success)
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

makeStates ["On", "Off", "AnyState"]
makeEvents ["TakeOn"]

stateMachinTest1 :: IO Bool
stateMachinTest1 = finishFor 100 $ do
    success <- newFlag
    sm  <- runStateMachine logOff Off $ do
        groupStates    AnyState $ On <: Off <: []
        addTransition  AnyState TakeOn On
        entryDo        On $ liftFlag success
        setFinishState On
    emit sm TakeOn
    wait success

main :: IO ()
main = do
    putTextLn ""
    hspec $ do
        it "Actor ping pong test"     $ isOk actorPingPongTest
        it "Test 1 for state machine" $ isOk stateMachinTest1

