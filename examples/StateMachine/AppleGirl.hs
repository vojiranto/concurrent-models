{-# LANGUAGE TemplateHaskell #-}
module StateMachine.AppleGirl where

import           Universum
import           Control.Concurrent.Model
import           Control.Concurrent.Flag

makeEvents ["Apple"]
makeStates ["Hungry", "WellFed"]
makeFsm "AppleGirl" [[t|Apple|]]

appleGirl :: Loger -> IO ()
appleGirl loger = do
    girlIsWellFed <- newFlag
    girl          <- runAppleGirl loger girlIsWellFed
    feed girl 3 Apple
    wait girlIsWellFed

runAppleGirl :: Loger -> Flag -> IO AppleGirl
runAppleGirl loger girlIsWellFed = runFsm loger Hungry $ do
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
