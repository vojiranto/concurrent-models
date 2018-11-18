module StateMachine.Sequential where

import           Universum
import           Control.Loger
import           Control.StateMachine
import           Data.Flag

data S1 = S1
data S2 = S2 
data S3 = S3

data Event = Event

sequentialStateMachine :: IO ()
sequentialStateMachine = do
    stopSM <- newFlag
    sm     <- runStateMachine logOff S1 $ do
        addTransition  S1 Event S2
        addTransition  S2 Event S3
        setFinishState S3
        exitDo S3 $ liftFlag stopSM
    sm `emit` Event
    sm `emit` Event
    wait stopSM