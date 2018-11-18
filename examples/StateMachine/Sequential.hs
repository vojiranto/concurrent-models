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
    sm     <- runStateMachine logOf S1 $ do
        addTransition  S1 Event S2
        addTransition  S2 Event S3
        setFinishState S3
        exitDo S3 $ liftFlag stopSM
    sm `emit` Event
    sm `emit` Event
    wait stopSM

{-
-- log of example.
[1118SuUnP1] [set transition] [state S1] -> [event Event] -> [state S2]
[SM] [1118SuUnP1] [set transition] [state S2] -> [event Event] -> [state S3]
[SM] [1118SuUnP1] [set finish state] [state S3]
[SM] [1118SuUnP1] [set 'exit do' handler] [state S3]
[SM] [1118SuUnP1] [init state] [state S1]
[SM] [1118SuUnP1] [transition] [state S1] -> [event Event] -> [state S2]
[SM] [1118SuUnP1] [transition] [state S2] -> [event Event] -> [state S3]
[SM] [1118SuUnP1] [finish state] [state S3]
[SM] [1118SuUnP1] [exit do] [state S3]
-}