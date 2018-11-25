module Actor.Postman where

import           Universum

import           Data.Flag        -- To report about successful completion.
import           Control.Concurrent.Loger
import           Control.Concurrent.Actor

newtype Subscription a = Subscription (a -> IO ())
data Times  = Times
data WSPost = WSPost

postmanExample :: IO ()
postmanExample = do
    postman       <- runPostman
    wsAccepted    <- newFlag
    subs1         <- runSubscriberWSPost wsAccepted
    timesAccepted <- newFlag
    subs2         <- runSubscriberTimes timesAccepted

    postman `notify` (Subscription (subs1 `notify`) :: Subscription WSPost)
    postman `notify` (Subscription (subs2 `notify`) :: Subscription Times)

    postman `notify` Times
    postman `notify` WSPost

    wait wsAccepted
    wait timesAccepted

runPostman :: IO Actor
runPostman = runActor logOff $ do
    makeDistribution ([] :: [Subscription WSPost])
    makeDistribution ([] :: [Subscription Times])
    where
        makeDistribution :: Typeable a => [Subscription a] -> ActorL ()
        makeDistribution subscribers = do
            subscribers' <- liftIO $ newIORef subscribers
            math $ acceptSubscription subscribers'
            math $ resending subscribers'

        acceptSubscription :: IORef [Subscription a] -> Subscription a -> IO ()
        acceptSubscription ref newSubs = modifyIORef ref (newSubs:)

        resending :: IORef [Subscription a] -> a -> IO ()
        resending ref msg = do
            subscribers' <- readIORef ref
            forM_ subscribers' $ \(Subscription s) -> s msg

runSubscriberWSPost :: Flag -> IO Actor
runSubscriberWSPost flag = runActor logOff $
    math $ \WSPost -> liftFlag flag

runSubscriberTimes :: Flag -> IO Actor
runSubscriberTimes flag = runActor logOff $
    math $ \Times -> liftFlag flag