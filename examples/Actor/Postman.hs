module Actor.Postman where

import           Universum
import           Data.Dynamic
import qualified Data.Map                                                   as M

--import           Data.Flag        -- To report about successful completion.
--import           Control.Concurrent.Loger
import           Control.Concurrent.Actor

newtype Notify = Notify Dynamic

packNotify :: Typeable a => (a -> IO ()) -> Notify
packNotify = Notify . toDyn


newtype Subscribers = Subscribers (M.Map EventType (M.Map TextId Notify))

broadcast' :: Typeable msg => Subscribers -> msg -> IO ()
broadcast' (Subscribers subscribers) msg =
    whenJust (rawDataToType msg `M.lookup` subscribers) $
        \broadcastList -> forM_ (M.elems broadcastList) $
            \(Notify dyn) -> whenJust (fromDynamic dyn) ($ msg)

emptySubscribers :: Subscribers
emptySubscribers = Subscribers mempty

addSubscriber' :: Subscription -> Subscribers -> Subscribers
addSubscriber' (Subscription textId eventType act) (Subscribers subscribers)
    = Subscribers (M.alter upd eventType subscribers)
    where
        upd (Just a) = Just (M.insert textId act a)
        upd _        = Just (M.singleton textId act)

deleteSubscriber' :: Unsubscribe -> Subscribers -> Subscribers
deleteSubscriber' (Unsubscribe textId eventType) (Subscribers subscribers)
    = Subscribers (M.alter upd eventType subscribers)
    where
        upd (Just a) = Just (M.delete textId a)
        upd _        = Nothing

{-
subscriptioService :: ActorL (IORef Subscribers)
subscriptioService = do
-}

postmanExample :: IO ()
postmanExample = pure ()


data Subscription = Subscription TextId EventType Notify
data Unsubscribe  = Unsubscribe  TextId EventType

{-
makeDistribution :: Typeable a => [Subscription a] -> ActorL ()
makeDistribution subscribers = do
    subscribers' <- liftIO $ newIORef subscribers
    math $ acceptSubscription subscribers'
    math $ resending subscribers'

    acceptSubscription :: IORef [Subscription a] -> Subscription a -> IO ()
    acceptSubscription ref newSubs = modifyIORef ref (newSubs:)
-}

{-
data Times  = Times
data WSPost = WSPost

postmanExample :: IO ()
postmanExample = do
    postman       <- runPostman

    wsAccepted    <- newFlag
    subs1         <- runSubscriberWSPost wsAccepted
    postman `notify` (Subscription (subs1 `notify`) :: Subscription WSPost)

    timesAccepted <- newFlag
    subs2         <- runSubscriberTimes timesAccepted
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
-}