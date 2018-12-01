{-# Language FlexibleContexts #-}
module Actor.Postman where

import           Universum
import           Data.Flag        -- To report about successful completion.
import           Control.Concurrent.Loger
import           Control.Concurrent.Actor
import           Control.Concurrent.Service.Subscription 

data Times  = Times
data WSPost = WSPost

runPostman :: Loger -> IO Actor
runPostman loger = runActor loger $ do
    subscribers <- subscriptioService loger
    math $ \Times  -> multicast subscribers Times
    math $ \WSPost -> multicast subscribers WSPost

postmanExample :: IO ()
postmanExample = do
    postman       <- runPostman logToConsole

    wsAccepted    <- newFlag
    subs1         <- runSubscriberWSPost wsAccepted
    postman `notify` makeNotify subs1 (\(msg :: WSPost) -> notify subs1 msg)

    timesAccepted <- newFlag
    subs2         <- runSubscriberTimes timesAccepted
    postman `notify` makeNotify subs2 (\(msg :: Times) -> notify subs2 msg) 

    postman `notify` Times
    postman `notify` WSPost

    wait wsAccepted
    wait timesAccepted

runSubscriberWSPost :: Flag -> IO Actor
runSubscriberWSPost flag = runActor logOff $
    math $ \WSPost -> liftFlag flag

runSubscriberTimes :: Flag -> IO Actor
runSubscriberTimes flag = runActor logOff $
    math $ \Times -> liftFlag flag
