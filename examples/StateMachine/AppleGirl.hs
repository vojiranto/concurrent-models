{-# LANGUAGE TemplateHaskell #-}
module StateMachine.AppleGirl where

import           Universum
import           Control.Concurrent.Loger
import           Control.Concurrent.StateMachine
import           Data.Flag


makeEvents ["Apple"]
makeStates ["Hungry", "WellFed"]
-- TODO:
-- makeFSM "AppleGirl" ["Apple"]

-- begin make makeFSM
newtype AppleGirl = AppleGirl StateMachine

instance Fsm AppleGirl where
    runFsm logerAction initState machineDescriptione =
        AppleGirl <$> runFsm logerAction initState machineDescriptione

    readState (AppleGirl fsm) = readState fsm

instance Listener AppleGirl Apple where
    notify        (AppleGirl fsm) = notify        fsm
    notifyAndWait (AppleGirl fsm) = notifyAndWait fsm
-- end make makeFSM

appleGirl :: IO ()
appleGirl = do
    stopSM <- newFlag
    girl :: AppleGirl <- runFsm logOff Hungry $ do
        eatenApples <- makeAppleCounter
        staticalDo Hungry $ eatApple eatenApples

        addConditionalTransition Hungry $ \Apple -> do
            applesCounter <- readIORef eatenApples
            if applesCounter >= 3 then just WellFed else nothing

        addFinalState WellFed
        exitDo WellFed $ liftFlag stopSM

    forM_ [1..3 :: Int] $ \_ -> girl `notify` Apple
    
    wait stopSM

makeAppleCounter :: StateMachineL (IORef Integer)
makeAppleCounter = liftIO $ newIORef 0

eatApple :: (Num a, MonadIO m) => IORef a -> Apple -> m ()
eatApple eatenApples Apple = modifyIORef eatenApples (+1)

