module StateMachine.Sequential where

import           Universum
import           Control.Concurrent.Model
import           Control.Concurrent.Loger
import           Control.Concurrent.StateMachine
import           Control.Concurrent.Flag

data S1 = S1
data S2 = S2 
data S3 = S3

data EkzampleEvent = EkzampleEvent

sequentialStateMachine :: IO ()
sequentialStateMachine = do
    stopSM <- newFlag
    sm    <- runStateMachine logOff S1 $ do
        ifE EkzampleEvent $ S1 >-> S2
        ifE EkzampleEvent $ S2 >-> S3
        addFinalState S3
        onExit S3 $ liftFlag stopSM
    sm `notify` EkzampleEvent
    sm `notify` EkzampleEvent
    wait stopSM