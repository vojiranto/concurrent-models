{-# Language TemplateHaskell #-}

import           Universum
import           Control.Loger
import           Control.StateMachine

makeStates ["Open", "Closed"]
makeEvents ["Close"]

main :: IO ()
main = do
    door <- runStateMachine logToConsole Closed $ do
        addTransition  Open   Close Closed
        addTransition  Closed Open  Open

        staticalDo     Closed $ \Close -> putTextLn "Open door?"
        entryDo        Closed $ putTextLn "Open door?"
        exitDo         Closed $ putTextLn "Door is now Open"

        staticalDo     Open $ \Open -> putTextLn "The door must be closed. OK?"
        entryDo        Open $ putTextLn "The door must be closed. OK?"
        exitDo         Open $ putTextLn "Door is now Closed"
    
    readComand door

readComand :: StateMachine -> IO ()
readComand sm = do
    ln <- getLine
    case ln of
        "open"  -> emit sm Open  >> readComand sm 
        "close" -> emit sm Close >> readComand sm 
        "exit"  -> pure ()
        _       -> readComand sm
