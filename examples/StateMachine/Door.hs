module StateMachine.Door where

import           Universum
import           Control.Concurrent.Loger
import           Control.Concurrent.StateMachine

data Open   = Open
data Close  = Close
data Closed = Closed

exampleDoor :: IO ()
exampleDoor = readDoorComands =<< makeDoor

makeDoor :: IO StateMachine
makeDoor = runStateMachine logToConsole Closed $ do
    addTransition  Open   Close Closed
    addTransition  Closed Open  Open

    staticalDo     Closed $ \Close -> putTextLn "Open door?"
    entryDo        Closed $ putTextLn "Open door?"
    exitDo         Closed $ putTextLn "Door is now Open"

    staticalDo     Open $ \Open -> putTextLn "The door must be closed. OK?"
    entryDo        Open $ putTextLn "The door must be closed. OK?"
    exitDo         Open $ putTextLn "Door is now Closed"

readDoorComands :: StateMachine -> IO ()
readDoorComands sm = do
    ln <- getLine
    case ln of
        "open"  -> notify sm Open  >> readDoorComands sm 
        "close" -> notify sm Close >> readDoorComands sm 
        "exit"  -> pure ()
        _       -> readDoorComands sm
