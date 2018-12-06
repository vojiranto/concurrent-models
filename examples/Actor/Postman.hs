{-# Language FlexibleContexts #-}
{-# Language TemplateHaskell  #-}
{-# Language QuasiQuotes      #-}
module Actor.Postman where

import           Universum
import           Control.Concurrent.Flag        -- To report about successful completion.
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

postmanExample1 :: IO ()
postmanExample1 = do
    postman       <- runPostman logOff

    wsAccepted    <- newFlag
    timesAccepted <- newFlag

    subs1         <- runActor logOff $ do
        math $ \WSPost -> liftFlag wsAccepted
        math $ \Times  -> liftFlag timesAccepted

    $(subscribe [t|WSPost|]) postman subs1
    $(subscribe [t|Times|])  postman subs1

    postman `notify` Times
    postman `notify` WSPost

    wait wsAccepted
    wait timesAccepted

postmanExample2 :: IO Bool
postmanExample2 = do
    postman <- runPostman logOff
    res     <- newEmptyMVar

    -- subscriber
    subs    <- runActor logOff $ do
        math (\WSPost -> putMVar res False :: IO ())
        math (\Times  -> putMVar res True  :: IO ())

    $(subscribe [t|WSPost|]) postman subs
    $(subscribe [t|Times|])  postman subs
    unsubscribe WSPost postman subs

    postman `notify` WSPost
    postman `notify` Times
    readMVar res
