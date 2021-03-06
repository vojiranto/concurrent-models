{-# Language TemplateHaskell #-}
module Main where

import           Universum
import           Control.Concurrent
import           Testing.Hspec.Extra

import           Tests.BroadcastServer
import           Tests.Serialization

import           Control.Concurrent.Model
import           Control.Concurrent.Node.Loger
import           Control.Concurrent.Flag
import           Control.Concurrent.Service.Serialization.Adt
import           Control.Concurrent.Service.Serialization.Binary

import           Tcp
import           StateMachine
import           Actor

makeStates ["On", "Off", "AnyState"]
makeEvents ["TakeOn"]

stateMachinTest1 :: Loger -> IO Bool
stateMachinTest1 loger' = finishFor 100 $ do
    success <- newFlag
    sm <- runStateMachine loger' Off $ do
        groupStates  AnyState $ On <: Off <: []
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
    logActor <- runLogerActor
    void $ сonsoleLogOn logActor
    putTextLn ""
    hspec $ do
        it "Actor ping pong test"
            $ isOk $ finishFor 5000 $ actorPingPong $ loger logActor
        
        it "Actor postman test (subscription)"
            $ isOk $ finishFor 5000 $ postmanExample1 $ loger logActor

        it "Actor postman test (unscribe)"
            $ isOk $ postmanExample2 $ loger logActor

        it "Tcp connection test"
            $ isOk $ finishFor 10000 $ tcpExample (loger logActor) 5000 50

        it "Tcp broadcast server test"
            $ isOk $ finishFor 10000 $ broadcastServerTest (loger logActor) 5001 50

        it "Tcp serialization binary test "
            $ isOk $ finishFor 10000 $ serializationTest Bin (loger logActor)

        it "Tcp serialization Adt test "
            $ isOk $ finishFor 10000 $ serializationTest Adt (loger logActor)
            
        it "Test 1 for state machine"
            $ isOk $ stateMachinTest1 $ loger logActor

        it "Test 2 for state machine"
            $ isOk $ finishFor 1000 $ sequentialStateMachine $ loger logActor

        it "Test 3 for state machine"
            $ isOk $ finishFor 1000 $ groupingStateMachine $ loger logActor

        it "Test 4 for state machine"
            $ isOk $ finishFor 1000 $ appleGirl $ loger logActor


    threadDelay 1000000