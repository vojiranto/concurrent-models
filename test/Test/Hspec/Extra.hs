module Test.Hspec.Extra (module X, finishFor, isOk) where

import           Universum
import           Test.Hspec as X
import           Control.Concurrent
import           Control.Concurrent.Async (race)

finishFor :: Int -> IO () -> IO Bool
finishFor time action = isRight <$> race (threadDelay time) action

isOk :: IO Bool -> Expectation
isOk action = shouldReturn action True