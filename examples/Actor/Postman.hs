{-# Language FlexibleContexts #-}
module Actor.Postman where

import           Universum
import           Data.Dynamic
import qualified Data.Map                                                   as M
import           Data.Flag        -- To report about successful completion.
import           Control.Concurrent.Loger
import           Control.Concurrent.Actor

newtype Notify = Notify Dynamic

packNotify :: Typeable a => (a -> IO ()) -> Notify
packNotify = Notify . toDyn

newtype Subscribers = Subscribers (M.Map EventType (M.Map TextId Notify))

data Subscription = Subscription TextId EventType Notify
data Unsubscribe  = Unsubscribe  TextId EventType

multicast :: Typeable msg => IORef Subscribers -> msg -> IO ()
multicast subscribers msg = do
    broadcastList <- readIORef subscribers
    multicast' broadcastList 
    where
        multicast' :: Subscribers -> IO ()
        multicast' (Subscribers subscribers') =
            whenJust (rawDataToType msg `M.lookup` subscribers') $
                \broadcastList -> forM_ (M.elems broadcastList) $
                    \(Notify dyn) -> whenJust (fromDynamic dyn) ($ msg)

subscriptioService
    :: (Math (Unsubscribe -> IO ()) m
    ,   Math (Subscription -> IO ()) m
    ,   MonadIO m
    ,   This m act
    ,   HaveTextId act) =>
    Loger -> m (IORef Subscribers)
subscriptioService loger = do
    act <- this
    subscribers <- liftIO $ newIORef emptySubscribers
    math $ subsription subscribers
    math $ unsubscribe subscribers
    liftIO $ loger $ "\n[Subscription service] " <> describe (getTextId act) <> " Is init"
    pure subscribers
    where
        emptySubscribers :: Subscribers
        emptySubscribers = Subscribers mempty

        subsription :: IORef Subscribers -> Subscription -> IO ()
        subsription subscribers subs = modifyIORef' subscribers (addSubscriber subs)
            where
                addSubscriber :: Subscription -> Subscribers -> Subscribers
                addSubscriber (Subscription textId eventType act) (Subscribers subscribers')
                    = Subscribers (M.alter upd eventType subscribers')
                    where
                        upd (Just a) = Just (M.insert textId act a)
                        upd _        = Just (M.singleton textId act)

        unsubscribe :: IORef Subscribers -> Unsubscribe -> IO ()
        unsubscribe subscribers unsub = modifyIORef' subscribers (deleteSubscriber unsub)
            where
                deleteSubscriber :: Unsubscribe -> Subscribers -> Subscribers
                deleteSubscriber (Unsubscribe textId eventType) (Subscribers subscribers')
                    = Subscribers (M.alter upd eventType subscribers')
                    where
                        upd (Just a) = Just (M.delete textId a)
                        upd _        = Nothing

data Times  = Times
data WSPost = WSPost

runPostman :: Loger -> IO Actor
runPostman loger = runActor loger $ do
    subscribers <- subscriptioService loger
    math $ \Times  -> multicast subscribers Times
    math $ \WSPost -> multicast subscribers WSPost

makeNotify :: (Typeable a2, HaveTextId a1) => a1 -> (a2 -> IO ()) -> Subscription
makeNotify subs act = Subscription (getTextId subs) (actionToType act) (packNotify act)

postmanExample :: IO ()
postmanExample = do
    postman       <- runPostman logOff

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
