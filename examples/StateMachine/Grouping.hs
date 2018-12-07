module StateMachine.Grouping where

import           Universum
import           Control.Concurrent.Model
import           Control.Concurrent.Loger
import           Control.Concurrent.StateMachine
import           Control.Concurrent.Flag

data S1 = S1
data S2 = S2 
data S3 = S3
data G  = G
data FS = FS

data Move = Move
data Exit = Exit

groupingStateMachine :: IO ()
groupingStateMachine = do
    stopSM <- newFlag
    sm <- runStateMachine logOff S1 $ do
        ifE Move $ S1 >-> S2
        ifE Move $ S2 >-> S3
        ifE Move $ S3 >-> S1

        groupStates    G $ S1 <: S2 <: S3 <: []
        ifE Exit     $ G >-> FS

        addFinalState FS
        onExit FS $ liftFlag stopSM

    sm `notify` Move
    sm `notify` Exit

    wait stopSM