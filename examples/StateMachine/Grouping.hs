module StateMachine.Grouping where

import           Universum
import           Control.Loger
import           Control.StateMachine
import           Data.Flag

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
    sm     <- runStateMachine logOff S1 $ do
        addTransition  S1 Move S2
        addTransition  S2 Move S3
        addTransition  S3 Move S1

        groupStates    G $ S1 <: S2 <: S3 <: []
        addTransition  G Exit FS
        exitDo         G $ pure ()

        setFinishState FS
        exitDo FS $ liftFlag stopSM

    sm `emit` Move
    sm `emit` Exit

    wait stopSM