{-# LANGUAGE TemplateHaskell #-}
module StateMachine.AppleGirl where

import           Universum
import           Control.Concurrent.Loger
import           Control.Concurrent.StateMachine
import           Data.Flag


makeEvents ["Apple"]
makeStates ["Hungry", "WellFed"]
makeFsm "AppleGirl" ["Apple"]

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

    replicateM_ 3 $ girl `notify` Apple

    wait stopSM

makeAppleCounter :: StateMachineL (IORef Integer)
makeAppleCounter = liftIO $ newIORef 0

eatApple :: (Num a, MonadIO m) => IORef a -> Apple -> m ()
eatApple eatenApples Apple = modifyIORef eatenApples (+1)

