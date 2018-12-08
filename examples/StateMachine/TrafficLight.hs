module StateMachine.TrafficLight where

import           Universum
import           Control.Concurrent.Model
import           Control.Concurrent.Loger

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
makeTrafficLight1 = runStateMachine loger Green $ do
    ifE ChangeColor $ Green      >-> YellowUp
    ifE ChangeColor $ YellowUp   >-> Red
    ifE ChangeColor $ Red        >-> YellowDown
    ifE ChangeColor $ YellowDown >-> Green

-- properly functioning traffic lights
makeTrafficLight2 :: IO StateMachine
makeTrafficLight2 = runStateMachine loger Green $ do
    -- add to fsm transitions from one states to another.
    ifE ChangeColor $ Green >-> Yellow
    ifE ChangeColor $ Red   >-> Yellow
    
    -- during the construction of the FSN, you can use IO.
    directionSM <- liftIO $ runStateMachine loger Red $ do
        ifE ChangeColor $ Green >-> Red
        ifE ChangeColor $ Red   >-> Green
    
    onExit Red   $ notifyAndWait directionSM ChangeColor
    onExit Green $ notifyAndWait directionSM ChangeColor

    -- add context dependent transition
    -- naturally, you can also use logical conditions for events.
    Yellow >?> \ChangeColor -> Just <$> readState directionSM

-- read lines have not yet read the "exit"
readLightCommand :: StateMachine -> IO ()
readLightCommand sm = do
    line <- getLine
    when (line /= "exit") $ do
        notify sm ChangeColor
        readLightCommand sm
