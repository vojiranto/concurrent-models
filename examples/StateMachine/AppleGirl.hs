{-# LANGUAGE TemplateHaskell #-}
module StateMachine.AppleGirl where

import           Universum
import           Control.Concurrent.Model
import           Control.Concurrent.Flag
import           Control.Concurrent.Loger

makeEvents ["Apple"]
makeStates ["Hungry", "WellFed"]
makeFsm "AppleGirl" ["Apple"]

appleGirl :: IO ()
appleGirl = do
    girlIsWellFed <- newFlag
    girl          <- runAppleGirl girlIsWellFed
    feed girl 3 Apple
    wait girlIsWellFed

runAppleGirl :: Flag -> IO AppleGirl
runAppleGirl girlIsWellFed = runFsm loger Hungry $ do
    toLog Trace "Init apple girl"
    hangryGirl
    wellFedGirl girlIsWellFed

hangryGirl :: StateMachineL ()
hangryGirl = do
    eatenApples <- makeAppleCounter
    mathS Hungry $ eatApple  eatenApples
    Hungry >?> toWellFed eatenApples

toWellFed :: IORef Integer -> Apple -> IO (Maybe MachineState)
toWellFed eatenApples _ = do
    applesCounter <- readIORef eatenApples
    if applesCounter >= 3 then just WellFed else nothing

makeAppleCounter :: StateMachineL (IORef Integer)
makeAppleCounter = liftIO $ newIORef 0

eatApple :: (Num a, MonadIO m) => IORef a -> Apple -> m ()
eatApple eatenApples Apple = modifyIORef eatenApples (+1)
        
wellFedGirl :: Flag -> StateMachineL ()
wellFedGirl girlIsWellFed = do
    addFinalState WellFed
    onExit WellFed $ liftFlag girlIsWellFed

feed :: AppleGirl -> Int -> Apple -> IO ()
feed girl num apple = replicateM_ num $ girl `notify` apple
