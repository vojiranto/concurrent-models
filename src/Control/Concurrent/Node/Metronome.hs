{-# Language TemplateHaskell  #-}
module Control.Concurrent.Node.Metronome where

import           Control.Concurrent.Prelude
import           Control.Concurrent.Service.Subscription
import           Control.Concurrent.Model
import           System.Clock

makeStates ["Start", "Stop", "Started", "Stoped", "Time"]
makeFsm "Metronome" [[t|Stop|], [t|Subscription|], [t|Unsubscribe|]]

metronome :: Typeable msg => Loger -> Integer -> msg -> IO Metronome
metronome loger timeout event = do
    Metronome fsm <- runFsm loger Started $ do
        toLog Info "Start of metronome"
        subscribers <- subscriptioService
        startTime   <- liftIO $ getTime Realtime
        stepСounter <- liftIO $ newIORef (0 :: Integer)
        myRef <- this
        math $ \Time -> do
            step <- atomicModifyIORef' stepСounter (\step -> (step + 1, step))
            void $ forkIO $ do
                multicast subscribers event
                сurrentTime <- getTime Realtime
                let timeOfWork = toNanoSecs (diffTimeSpec сurrentTime startTime)
                let lateness   = timeOfWork - step * timeout
                threadDelay $ fromEnum $ timeout - lateness
                notify myRef Time
        addFinalState Stoped
        ifE Stop $ Started >-> Stoped
    notify fsm Time
    pure $ Metronome fsm
