module Main where

import           Universum
import           StateMachine.TrafficLight

main :: IO ()
main = readLightCommand =<< makeTrafficLight2
