module StateMachine.Sequential where

import           Universum
import           Control.Concurrent.Loger
import           Control.Concurrent.StateMachine
import           Data.Flag

data S1 = S1
data S2 = S2 
data S3 = S3

data Event = Event

sequentialStateMachine :: IO ()
sequentialStateMachine = do
    stopSM <- newFlag
    sm <- runStateMachine logOff S1 $ do
        addTransition  S1 Event S2
        addTransition  S2 Event S3
        addFinalState S3
        exitDo S3 $ liftFlag stopSM
    sm `notify` Event
    sm `notify` Event
    wait stopSM