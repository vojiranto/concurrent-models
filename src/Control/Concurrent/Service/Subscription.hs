{-# Language FlexibleContexts #-}
{-# Language TemplateHaskell  #-}
{-# Language QuasiQuotes      #-}
module Control.Concurrent.Service.Subscription
    ( Notify
    , Subscription
    , subscriptioService
    , makeNotify
    , unsubscribe
    , subscribe
    , multicast
    ) where

import           Universum hiding (Type)
import           Data.Dynamic
import qualified Data.Map                                                   as M
import           Control.Concurrent.Loger
import           Control.Concurrent.Actor
import           Language.Haskell.TH

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
                    \(Notify dyn') -> whenJust (fromDynamic dyn') ($ msg)

subscriptioService
    :: (Math (Unsubscribe -> IO ()) m
    ,   Math (Subscription -> IO ()) m
    ,   MonadIO m
    ,   This m act
    ,   HaveTextId act) =>
    Loger -> m (IORef Subscribers)
subscriptioService loger = do
    textId <- getTextId <$> this
    subscribers <- liftIO $ newIORef emptySubscribers
    math $ subsription textId subscribers
    math $ unsubscribe' textId subscribers
    liftIO $ loger $ "[Subscription service] " <> describe textId <> " Is init"
    pure subscribers
    where
        emptySubscribers :: Subscribers
        emptySubscribers = Subscribers mempty

        subsription :: TextId -> IORef Subscribers -> Subscription -> IO ()
        subsription serviceId subscribers subs@(Subscription textId' eventType' _) = do
            loger $ "[Subscription service] " <> describe serviceId
                <> " [subsription] " <> describe textId' <> " "
                <> describe eventType'
            modifyIORef' subscribers (addSubscriber subs)
            where
                addSubscriber :: Subscription -> Subscribers -> Subscribers
                addSubscriber (Subscription textId eventType act) (Subscribers subscribers')
                    = Subscribers (M.alter upd eventType subscribers')
                    where
                        upd (Just a) = Just (M.insert textId act a)
                        upd _        = Just (M.singleton textId act)

        unsubscribe' :: TextId -> IORef Subscribers -> Unsubscribe -> IO ()
        unsubscribe' serviceId subscribers unsub@(Unsubscribe textId' eventType') = do
            loger $ "[Subscription service] " <> describe serviceId
                <> " [unsubscribe] " <> describe textId' <> " "
                <> describe eventType'
            modifyIORef' subscribers (deleteSubscriber unsub)
            where
                deleteSubscriber :: Unsubscribe -> Subscribers -> Subscribers
                deleteSubscriber (Unsubscribe textId eventType) (Subscribers subscribers')
                    = Subscribers (M.alter upd eventType subscribers')
                    where
                        upd (Just a) = Just (M.delete textId a)
                        upd _        = Nothing

-- $(subscribe "WSPost") postman subs
subscribe :: Q Type -> ExpQ
subscribe typeName =
    [e| \postman subs -> postman `notify` makeNotify subs (\(msg :: $(typeName)) -> notify subs msg) |]


makeNotify :: (Typeable a2, HaveTextId a1) => a1 -> (a2 -> IO ()) -> Subscription
makeNotify subs act = Subscription (getTextId subs) (actionToType act) (packNotify act)

unsubscribe
    :: (HaveTextId a1, Typeable a2, Listener a Unsubscribe)
    => a2 -> a -> a1 -> IO ()
unsubscribe msg postman subs = postman `notify` makeUnsubscribe subs msg 

makeUnsubscribe :: (Typeable a2, HaveTextId a1) => a1 -> a2 -> Unsubscribe
makeUnsubscribe subs act = Unsubscribe (getTextId subs) (rawDataToType act)

