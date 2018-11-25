module StateMachine.TrafficLight where

import           Universum
import           Control.Concurrent.Loger
import           Control.Concurrent.StateMachine

-- states for traffic light
data Green      = Green
data Yellow     = Yellow 
data YellowUp   = YellowUp
data YellowDown = YellowDown
data Red        = Red

-- 
data ChangeColor = ChangeColor

-- simple but not the right example, we all know that there is three traffic
-- lights, not four lights?
makeTrafficLight1 :: IO StateMachine
makeTrafficLight1 = runStateMachine logToConsole Green $ do
    addTransition Green         ChangeColor YellowUp
    addTransition YellowUp      ChangeColor Red
    addTransition Red           ChangeColor YellowDown
    addTransition YellowDown    ChangeColor Green

-- properly functioning traffic lights
makeTrafficLight2 :: IO StateMachine
makeTrafficLight2 = runStateMachine logToConsole Green $ do
    -- add to fsm transitions from one states to another.
    addTransition Green  ChangeColor Yellow
    addTransition Red    ChangeColor Yellow
    
    -- during the construction of the FSN, you can use IO.
    directionSM <- liftIO $ runStateMachine logToConsole Red $ do
        addTransition Green  ChangeColor Red
        addTransition Red    ChangeColor Green
    
    exitDo Red   $ notifyAndWait directionSM ChangeColor
    exitDo Green $ notifyAndWait directionSM ChangeColor

    -- add context dependent transition
    -- naturally, you can also use logical conditions for events.
    addConditionalTransition Yellow $
        \ChangeColor -> Just <$> readState directionSM

-- read lines have not yet read the "exit"
readLightCommand :: StateMachine -> IO ()
readLightCommand sm = do
    line <- getLine
    when (line /= "exit") $ do
        notify sm ChangeColor
        readLightCommand sm
