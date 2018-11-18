{-# Language TemplateHaskell #-}
module Main where

import           Universum
import           Test.Hspec.Extra

import           Data.Flag
import           Control.Loger
import           Control.StateMachine
import           Control.StateMachine.Domain
import           Actor.PingPong

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
        it "Actor ping pong test"     $ isOk (finishFor 1000 actorPingPong)
        it "test1 for 'is'" $ (toMachineState On `is` On) `shouldBe` True
        it "test2 for 'is'" $ (On `is` toMachineState On) `shouldBe` True
        it "Test 1 for state machine" $ isOk stateMachinTest1