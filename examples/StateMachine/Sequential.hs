module StateMachine.Sequential where

import           Universum
import           Control.Concurrent.Model
import           Control.Concurrent.Flag

data SequentialState1 = SequentialState1
data SequentialState2 = SequentialState2 
data SequentialState3 = SequentialState3

data EkzampleEvent = EkzampleEvent

sequentialStateMachine :: Loger-> IO ()
sequentialStateMachine loger = do
    stopSM <- newFlag
    sm    <- runStateMachine loger SequentialState1 $ do
        ifE EkzampleEvent $ SequentialState1 >-> SequentialState2
        ifE EkzampleEvent $ SequentialState2 >-> SequentialState3
        addFinalState SequentialState3
        onExit SequentialState3 $ liftFlag stopSM
    sm `notify` EkzampleEvent
    sm `notify` EkzampleEvent
    wait stopSM