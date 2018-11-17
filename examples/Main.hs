{-# Language TemplateHaskell #-}

import           Universum
import           Control.Loger
import           Control.StateMachine

makeStates ["Open", "Closed"]
makeEvents ["Close"]

makeStates ["Green", "Yellow", "YellowUp", "YellowDown", "Red"]
makeEvents ["ChangeColor"]

trafficLightExample1 :: IO StateMachine
trafficLightExample1 = runStateMachine logToConsole Green $ do
    addTransition Green         ChangeColor YellowUp
    addTransition YellowUp      ChangeColor Red
    addTransition Red           ChangeColor YellowDown
    addTransition YellowDown    ChangeColor Green

trafficLightExample2 :: IO StateMachine
trafficLightExample2 = runStateMachine logToConsole Green $ do
    addTransition Green  ChangeColor Yellow
    addTransition Red    ChangeColor Yellow
    
    directionSM <- initialiseAction $ runStateMachine logToConsole Red $ do
        addTransition Green  ChangeColor Red
        addTransition Red    ChangeColor Green
    
    exitDo Red   $ emitAndWait directionSM ChangeColor
    exitDo Green $ emitAndWait directionSM ChangeColor

    addConditionalTransition Yellow $
        \ChangeColor -> Just <$> takeState directionSM


main :: IO ()
main = readLightCommand =<< trafficLightExample1

readLightCommand :: StateMachine -> IO ()
readLightCommand sm  = do
    void getLine
    emitAndWait sm ChangeColor
    readLightCommand sm

exampleDoor :: IO ()
exampleDoor = do
    door <- runStateMachine logToConsole Closed $ do
        addTransition  Open   Close Closed
        addTransition  Closed Open  Open

        staticalDo     Closed $ \Close -> putTextLn "Open door?"
        entryDo        Closed $ putTextLn "Open door?"
        exitDo         Closed $ putTextLn "Door is now Open"

        staticalDo     Open $ \Open -> putTextLn "The door must be closed. OK?"
        entryDo        Open $ putTextLn "The door must be closed. OK?"
        exitDo         Open $ putTextLn "Door is now Closed"
    
    readDoorComand door

readDoorComand :: StateMachine -> IO ()
readDoorComand sm = do
    ln <- getLine
    case ln of
        "open"  -> emit sm Open  >> readDoorComand sm 
        "close" -> emit sm Close >> readDoorComand sm 
        "exit"  -> pure ()
        _       -> readDoorComand sm


