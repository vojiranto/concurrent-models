{-# Language FlexibleContexts #-}
{-# Language TemplateHaskell  #-}
{-# Language QuasiQuotes      #-}
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
    postman       <- runPostman logOff

    wsAccepted    <- newFlag
    subs1         <- runSubscriberWSPost wsAccepted
    $(subscribe [t|WSPost|]) postman subs1

    timesAccepted <- newFlag
    subs2         <- runSubscriberTimes timesAccepted
    $(subscribe [t|Times|]) postman subs2

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

