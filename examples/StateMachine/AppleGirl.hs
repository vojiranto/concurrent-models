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
        hangryGirl
        wellFedGirl stopSM
    replicateM_ 3 $ girl `notify` Apple
    wait stopSM

hangryGirl :: StateMachineL ()
hangryGirl = do
    eatenApples <- makeAppleCounter
    staticalDo               Hungry $ eatApple  eatenApples
    addConditionalTransition Hungry $ toWellFed eatenApples

toWellFed :: IORef Integer -> Apple -> IO (Maybe MachineState)
toWellFed eatenApples _ = do
    applesCounter <- readIORef eatenApples
    if applesCounter >= 3 then just WellFed else nothing

makeAppleCounter :: StateMachineL (IORef Integer)
makeAppleCounter = liftIO $ newIORef 0

eatApple :: (Num a, MonadIO m) => IORef a -> Apple -> m ()
eatApple eatenApples Apple = modifyIORef eatenApples (+1)
        
wellFedGirl :: Flag -> StateMachineL ()
wellFedGirl stopSM = do
    addFinalState WellFed
    exitDo WellFed $ liftFlag stopSM
