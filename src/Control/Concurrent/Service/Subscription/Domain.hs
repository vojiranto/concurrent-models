module Control.Concurrent.Service.Subscription.Domain where

import           Universum 
import           Data.Dynamic
import qualified Data.Map                                                   as M
import           Data.Event
import           Data.TextId

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