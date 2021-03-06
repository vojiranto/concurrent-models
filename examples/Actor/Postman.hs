{-# Language FlexibleContexts #-}
{-# Language TemplateHaskell  #-}
{-# Language QuasiQuotes      #-}
module Actor.Postman where

import           Universum
import           Control.Concurrent.Model
import           Control.Concurrent.Flag        -- To report about successful completion.
import           Control.Concurrent.Service.Subscription 

data Times  = Times
data WSPost = WSPost

runPostman :: Loger -> IO Actor
runPostman loger = runActor loger $ do
    subscribers <- subscriptionService
    math $ \Times  -> multicast subscribers Times
    math $ \WSPost -> multicast subscribers WSPost

postmanExample1 :: Loger -> IO ()
postmanExample1 loger = do
    postman       <- runPostman loger

    wsAccepted    <- newFlag
    timesAccepted <- newFlag

    subs1         <- runActor dummyLoger $ do
        math $ \WSPost -> liftFlag wsAccepted
        math $ \Times  -> liftFlag timesAccepted

    $(subscribe [t|WSPost|]) postman subs1
    $(subscribe [t|Times|])  postman subs1

    postman `notify` Times
    postman `notify` WSPost

    wait wsAccepted
    wait timesAccepted

postmanExample2 :: Loger -> IO Bool
postmanExample2 loger = do
    postman <- runPostman loger
    res     <- newEmptyMVar

    -- subscriber
    subs    <- runActor loger $ do
        math (\WSPost -> putMVar res False :: IO ())
        math (\Times  -> putMVar res True  :: IO ())

    $(subscribe   [t|WSPost|]) postman subs
    $(subscribe   [t|Times|])  postman subs
    $(unsubscribe [t|WSPost|]) postman subs

    postman `notify` WSPost
    postman `notify` Times
    readMVar res
