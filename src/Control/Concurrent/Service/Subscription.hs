{-# Language FlexibleContexts #-}
{-# Language TemplateHaskell  #-}
{-# Language QuasiQuotes      #-}
module Control.Concurrent.Service.Subscription
    ( Notify
    , Subscribers
    , Subscription
    , Unsubscribe
    , subscriptioService
    , makeNotify
    , unsubscribe
    , subscribe
    , multicast
    ) where


import           Control.Concurrent.Prelude hiding (Type)
import           Control.Concurrent.Model.Core
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
    ,   Logers m
    ,   This m act
    ,   HaveTextId act) =>
    m (IORef Subscribers)
subscriptioService = do
    toLog Trace "[Subscription service] [init begin]"
    loger <- getLoger
    subscribers <- liftIO $ newIORef $ Subscribers mempty
    let serviceLoger :: Describe a => a -> IO ()
        serviceLoger obj = loger Trace $
            "[Subscription service] " <> describe obj
    math $ \subs -> do
        serviceLoger subs
        modifyIORef' subscribers (addSubscriber subs)

    math $ \unsub -> do
        serviceLoger unsub
        modifyIORef' subscribers (deleteSubscriber unsub)

    toLog Trace "[Subscription service] [init end]"
    pure subscribers

-- $(subscribe "WSPost") postman subs
subscribe :: Q Type -> ExpQ
subscribe typeName = [e|
    \postman subs ->
        postman `notify` makeNotify subs (\(msg :: $(typeName)) -> notify subs msg)
    |]

makeNotify :: (Typeable a2, HaveTextId a1) => a1 -> (a2 -> IO ()) -> Subscription
makeNotify subs act = Subscription (getTextId subs) (actionToType act) (packNotify act)

unsubscribe :: Q Type -> ExpQ
unsubscribe typeName = [e|
    \postman subs ->
        postman `notify` Unsubscribe (getTextId subs) (proxyToType (Proxy :: Proxy $(typeName)))
    |]
