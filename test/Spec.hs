{-# Language TemplateHaskell #-}
module Main where

import           Universum
import           Test.Hspec.Extra

import           Data.Flag
import           Control.Concurrent.Loger
import           Control.Concurrent.StateMachine

import           StateMachine.Sequential
import           StateMachine.Grouping
import           StateMachine.AppleGirl
import           Actor.PingPong

makeStates ["On", "Off", "AnyState"]
makeEvents ["TakeOn"]

stateMachinTest1 :: IO Bool
stateMachinTest1 = finishFor 100 $ do
    success <- newFlag
    sm :: StateMachine <- runStateMachine logOff Off $ do
        groupStates    AnyState $ On <: Off <: []
        addTransition  AnyState TakeOn On
        entryDo        On $ liftFlag success
        addFinalState On
    notify sm TakeOn
    wait success

main :: IO ()
main = do
    putTextLn ""
    hspec $ do
        it "Actor ping pong test"     $ isOk (finishFor 1000 actorPingPong)
        it "Test 1 for state machine" $ isOk stateMachinTest1
        it "Test 2 for state machine" $ isOk (finishFor 1000 sequentialStateMachine)
        it "Test 3 for state machine" $ isOk (finishFor 1000 groupingStateMachine)
        it "Test 4 for state machine" $ isOk (finishFor 1000 appleGirl)