module StateMachine.AppleGirl where

import           Universum
import           Control.Concurrent.Loger
import           Control.Concurrent.StateMachine
import           Data.Flag

data Apple   = Apple
data Hungry  = Hungry 
data WellFed = WellFed

appleGirl :: IO ()
appleGirl = do
    stopSM <- newFlag
    girl :: StateMachine <- runStateMachine logOff Hungry $ do
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

