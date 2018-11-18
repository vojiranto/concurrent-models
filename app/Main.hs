module Main where

import           Universum
import           StateMachine.Door
import           StateMachine.TrafficLight

main :: IO ()
main = readLightCommand =<< makeTrafficLight2
