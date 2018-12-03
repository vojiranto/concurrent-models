{-# Language FlexibleContexts #-}
{-# Language TemplateHaskell  #-}
{-# Language QuasiQuotes      #-}
module Control.Concurrent.Service.Subscription
    ( Notify
    , Subscription
    , Unsubscribe
    , subscriptioService
    , makeNotify
    , unsubscribe
    , subscribe
    , multicast
    ) where

import           Universum hiding (Type)
import           Control.Concurrent.Loger
import           Control.Concurrent.Actor
import           Language.Haskell.TH
import           Control.Concurrent.Service.Subscription.Domain

multicast :: Typeable msg => IORef Subscribers -> msg -> IO ()
multicast subscribers msg = do
    broadcastList <- readIORef subscribers
    forM_ (getNotifyList broadcastList msg) ($ msg)

subscriptioService
    :: (Math (Unsubscribe -> IO ()) m
    ,   Math (Subscription -> IO ()) m
    ,   MonadIO m
    ,   This m act
    ,   HaveTextId act) =>
    Loger -> m (IORef Subscribers)
subscriptioService loger = do
    textId <- getTextId <$> this
    subscribers <- liftIO $ newIORef $ Subscribers mempty
    let serviceLoger :: Describe a => a -> IO ()
        serviceLoger obj = loger $
            "[Subscription service] " <> describe textId <> " " <> describe obj
    math $ \subs -> do
        serviceLoger subs
        modifyIORef' subscribers (addSubscriber subs)

    math $ \unsub -> do
        serviceLoger unsub
        modifyIORef' subscribers (deleteSubscriber unsub)

    liftIO $ loger $ "[Subscription service] " <> describe textId <> " Is init"
    pure subscribers

-- $(subscribe "WSPost") postman subs
subscribe :: Q Type -> ExpQ
subscribe typeName =
    [e| \postman subs -> postman `notify` makeNotify subs (\(msg :: $(typeName)) -> notify subs msg) |]

makeNotify :: (Typeable a2, HaveTextId a1) => a1 -> (a2 -> IO ()) -> Subscription
makeNotify subs act = Subscription (getTextId subs) (actionToType act) (packNotify act)

unsubscribe
    :: (HaveTextId a1, Typeable a2, Listener a Unsubscribe)
    => a2 -> a -> a1 -> IO ()
unsubscribe msg postman subs = postman `notify` makeUnsubscribe msg 
    where
        makeUnsubscribe act = Unsubscribe (getTextId subs) (rawDataToType act)

