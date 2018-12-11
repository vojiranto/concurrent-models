{-# Language TemplateHaskell #-}
module Main where

import           Universum
import           Control.Concurrent
import           Test.Hspec.Extra

import           Control.Concurrent.Model
import           Control.Concurrent.Node.Loger
import           Control.Concurrent.Flag

import           Tcp
import           StateMachine
import           Actor

makeStates ["On", "Off", "AnyState"]
makeEvents ["TakeOn"]

stateMachinTest1 :: IO Bool
stateMachinTest1 = finishFor 100 $ do
    success <- newFlag
    sm <- runStateMachine loger Off $ do
        groupStates    AnyState $ On <: Off <: []
        ifE TakeOn $ AnyState >-> On
        onEntry On   acceptTake
        onEntry On $ liftFlag success
        addFinalState On
    notify sm TakeOn
    wait success

acceptTake :: TakeOn -> IO ()
acceptTake _ = pure ()

main :: IO ()
main = do
    void сonsoleLogOn
    putTextLn ""
    hspec $ do
        it "Actor ping pong test"               $ isOk (finishFor 5000 actorPingPong)
        it "Actor postman test (subscription)"  $ isOk (finishFor 5000 postmanExample1)
        it "Actor postman test (unscribe)"      $ isOk postmanExample2
        it "Tcp connection test"                $ isOk (finishFor 5000 tcpExample)
        it "Test 1 for state machine"           $ isOk stateMachinTest1
        it "Test 2 for state machine"           $ isOk (finishFor 1000 sequentialStateMachine)
        it "Test 3 for state machine"           $ isOk (finishFor 1000 groupingStateMachine)
        it "Test 4 for state machine"           $ isOk (finishFor 1000 appleGirl)
    threadDelay 100000