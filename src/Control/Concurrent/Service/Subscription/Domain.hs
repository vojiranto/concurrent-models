module Control.Concurrent.Service.Subscription.Domain where


import           Control.Concurrent.Prelude
import qualified Data.Map                                                   as M
import           Control.Concurrent.Model.Core.Data

newtype Notify = Notify Dynamic

packNotify :: Typeable a => (a -> IO ()) -> Notify
packNotify = Notify . toDyn

unpakNotify :: Typeable a => Notify -> Maybe (a -> IO ())
unpakNotify (Notify dyn) = fromDynamic dyn

newtype Subscribers = Subscribers (M.Map EventType (M.Map TextId Notify))

data Subscription = Subscription TextId EventType Notify
data Unsubscribe  = Unsubscribe  TextId EventType

getNotifyList :: Typeable msg => Subscribers -> msg -> [msg -> IO ()]
getNotifyList (Subscribers subscribers) msg =
    catMaybes $ unpakNotify <$> join notifyLists
    where
        notifyLists :: [[Notify]]
        notifyLists = M.elems <$> maybeToList subsMap

        subsMap :: Maybe (M.Map TextId Notify)
        subsMap = rawDataToType msg `M.lookup` subscribers

addSubscriber :: Subscription -> Subscribers -> Subscribers
addSubscriber (Subscription textId eventType' act) (Subscribers subscribers')
    = Subscribers (M.alter upd eventType' subscribers')
    where
        upd (Just a) = Just (M.insert textId act a)
        upd _        = Just (M.singleton textId act)


deleteSubscriber :: Unsubscribe -> Subscribers -> Subscribers
deleteSubscriber (Unsubscribe textId eventType') (Subscribers subscribers')
    = Subscribers (M.alter upd eventType' subscribers')
    where
        upd (Just a) = Just (M.delete textId a)
        upd _        = Nothing

instance Describe Subscription where
    describe (Subscription textId eventType' _) =
        "[subscription] " <> describe textId <> " " <> describe eventType'

instance Describe Unsubscribe where
    describe (Unsubscribe textId eventType') =
        "[unsubscribe] " <> describe textId <> " " <> describe eventType'
